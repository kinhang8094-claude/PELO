# PELO engine used by all applications.
#
# Model (PELO, with plain Elo as the special case K2 = 0):
#   P(A beats B) = 1 / (1 + 10^(-(r_A + beta_AB + H * h - r_B) / 400))
#   after a decisive game with result y (1 = A won):
#     r_A    += K  * (y - p),   r_B    -= K  * (y - p)
#     beta_AB += K2 * (y - p),  beta_BA -= K2 * (y - p)      (beta is antisymmetric)
#
# Two update schedules:
#   "sequential": games are processed one at a time in date order, exactly as
#                 PELO::run_pelo / PELO::get_negloglike_pelo do (checked in
#                 tests/test_engine.R). Used for the sports applications.
#   "period":     every game in a period is predicted from the ratings at the
#                 start of the period and all updates of the period are applied
#                 together. Needed when several games are settled by the same
#                 information (e.g. all pairwise contests of one month are
#                 settled by that month's returns); processing them one at a
#                 time would leak results into predictions.
#
# h is an optional home indicator with its own parameter H (the PELO package has
# no home term; with H = 0 the engine is exactly the package model).
# Draws/ties (y = NA) are predicted but do not update ratings.

library(compiler)

# Index the players of a games table once; returns integer ids and the player list.
index_players <- function(g) {
  players <- sort(unique(c(g$A, g$B)))
  list(i = match(g$A, players), j = match(g$B, players), players = players, np = length(players))
}

# One pass over the games with fixed parameters.
# Returns, for every game, the prediction p, the rating-difference part z
# (r_A + H h - r_B, without beta) and the pairwise term b used, plus the end state.
pelo_pass <- cmpfun(function(par, i, j, y, h, np, mode = "sequential", period = NULL, state = NULL,
                             s = NULL, ns = 0L) {
  K <- par[["K"]]; K2 <- par[["K2"]]; H <- par[["H"]]
  Ks <- if ("Ks" %in% names(par)) par[["Ks"]] else 0
  r <- if (is.null(state)) rep(1500, np) else state$r
  B <- if (is.null(state)) matrix(0, np, np) else state$B
  n <- length(i); p <- z <- b <- numeric(n)
  ij <- i + (j - 1L) * np                     # linear index of B[i, j]
  ji <- j + (i - 1L) * np                     # linear index of B[j, i]
  if (mode == "sequential" && !is.null(s) && Ks != 0) {
    # Optional surface (or other context) ratings: each player also has one
    # rating per surface, added to the overall rating and updated with rate Ks.
    S <- if (is.null(state$S)) matrix(0, np, ns) else state$S
    is_ <- i + (s - 1L) * np; js_ <- j + (s - 1L) * np
    for (k in seq_len(n)) {
      a <- i[k]; c <- j[k]
      zk <- r[a] + S[is_[k]] + H * h[k] - r[c] - S[js_[k]]; bk <- B[ij[k]]
      pk <- 1 / (1 + 10^(-(zk + bk) / 400))
      p[k] <- pk; z[k] <- zk; b[k] <- bk
      yk <- y[k]
      if (!is.na(yk)) {
        d <- yk - pk
        r[a] <- r[a] + K * d;  r[c] <- r[c] - K * d
        S[is_[k]] <- S[is_[k]] + Ks * d;  S[js_[k]] <- S[js_[k]] - Ks * d
        B[ij[k]] <- bk + K2 * d;  B[ji[k]] <- B[ji[k]] - K2 * d
      }
    }
    return(list(p = p, z = z, b = b, state = list(r = r, B = B, S = S)))
  }
  if (mode == "sequential") {
    for (k in seq_len(n)) {
      a <- i[k]; c <- j[k]
      zk <- r[a] + H * h[k] - r[c]; bk <- B[ij[k]]
      pk <- 1 / (1 + 10^(-(zk + bk) / 400))
      p[k] <- pk; z[k] <- zk; b[k] <- bk
      yk <- y[k]
      if (!is.na(yk)) {
        d <- yk - pk
        r[a] <- r[a] + K * d;  r[c] <- r[c] - K * d
        B[ij[k]] <- bk + K2 * d;  B[ji[k]] <- B[ji[k]] - K2 * d
      }
    }
  } else {
    rows <- split(seq_len(n), period)
    for (rr in rows) {
      zk <- r[i[rr]] + H * h[rr] - r[j[rr]]; bk <- B[ij[rr]]
      pk <- 1 / (1 + 10^(-(zk + bk) / 400))
      p[rr] <- pk; z[rr] <- zk; b[rr] <- bk
      ok <- !is.na(y[rr])
      if (any(ok)) {
        d <- (y[rr] - pk)[ok]
        dr <- numeric(np)
        dr <- dr + tabulate_w(i[rr][ok], d, np) - tabulate_w(j[rr][ok], d, np)
        r <- r + K * dr
        if (K2 != 0) {
          B[ij[rr][ok]] <- B[ij[rr][ok]] + K2 * d     # each pair appears once per period
          B[ji[rr][ok]] <- B[ji[rr][ok]] - K2 * d
        }
      }
    }
  }
  list(p = p, z = z, b = b, state = list(r = r, B = B))
})

# Weighted tabulate: sum of w by integer index (1..n).
tabulate_w <- function(idx, w, n) {
  out <- numeric(n); s <- rowsum(w, idx); out[as.integer(rownames(s))] <- s[, 1]; out
}

# Negative log-likelihood of decisive games, with the same 1e-6 floor as the PELO package.
nll <- function(p, y) {
  ok <- !is.na(y); q <- ifelse(y[ok] == 1, p[ok], 1 - p[ok]); -sum(log(pmax(q, 1e-6)))
}

# Fit K (and K2 for PELO, H if use_home) by maximum likelihood on the training games.
# Several starting points are tried; the best optimum is kept.
fit_model <- function(model, g, ix, train, mode = "sequential", use_home = FALSE,
                      upper_K = 300, upper_K2 = 300, fixed_K2 = NULL, surface = NULL) {
  use_s <- !is.null(surface)
  free <- c("K", if (use_s) "Ks", if (model == "pelo" && is.null(fixed_K2)) "K2", if (use_home) "H")
  lower <- c(K = 0, Ks = 0, K2 = 0, H = -300)[free]; upper <- c(K = upper_K, Ks = upper_K, K2 = upper_K2, H = 300)[free]
  full <- function(th) { p <- c(K = 0, K2 = 0, H = 0, Ks = 0); if (!is.null(fixed_K2)) p["K2"] <- fixed_K2; p[free] <- th; p }
  tr <- which(train)
  obj <- function(th) {
    out <- pelo_pass(full(th), ix$i[tr], ix$j[tr], g$y[tr], g$h[tr], ix$np, mode, g$period[tr],
                     s = if (use_s) surface$s[tr], ns = if (use_s) surface$ns else 0L)
    nll(out$p, g$y[tr])
  }
  starts <- expand.grid(K = c(5, 25), Ks = if (use_s) c(5, 25) else NA,
                        K2 = if (model == "pelo" && is.null(fixed_K2)) c(2, 20) else NA,
                        H = if (use_home) 40 else NA)[, free, drop = FALSE]
  best <- NULL
  for (s in seq_len(nrow(starts))) {
    o <- optim(unlist(starts[s, ]), obj, method = "L-BFGS-B", lower = lower, upper = upper)
    if (is.null(best) || o$value < best$value) best <- o
  }
  list(par = full(best$par), nll = best$value, n_train = sum(!is.na(g$y[tr])))
}

# Run a fitted model over all games (training then walk-forward test).
predict_all <- function(fit, g, ix, mode = "sequential", surface = NULL) {
  pelo_pass(fit$par, ix$i, ix$j, g$y, g$h, ix$np, mode, g$period,
            s = if (!is.null(surface)) surface$s, ns = if (!is.null(surface)) surface$ns else 0L)
}

# Profile likelihood over K2: refit K (and H) with K2 held at each value.
# Returns the training NLL relative to K2 = 0 (negative = better than Elo).
profile_K2 <- function(g, ix, train, mode, use_home, grid = c(0, 1, 2, 5, 10, 20, 40), surface = NULL) {
  v <- sapply(grid, function(k2) fit_model("pelo", g, ix, train, mode, use_home, fixed_K2 = k2, surface = surface)$nll)
  data.frame(K2 = grid, delta_nll = v - v[1])
}
