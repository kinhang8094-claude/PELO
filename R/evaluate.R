# Evaluation helpers shared by the applications.

logloss_vec <- function(p, y) { q <- ifelse(y == 1, p, 1 - p); -log(pmax(q, 1e-6)) }

# Test-set metrics on decisive games
metrics <- function(p, y) {
  ok <- !is.na(y) & !is.na(p); p <- p[ok]; y <- y[ok]
  auc <- if (length(unique(y)) == 2)
    as.numeric(pROC::auc(pROC::roc(y, p, quiet = TRUE, direction = "<"))) else NA
  c(n = length(y), logloss = mean(logloss_vec(p, y)), brier = mean((p - y)^2),
    accuracy = mean((p > 0.5) == (y == 1)), auc = auc)
}

# Bootstrap CI for the mean of x (iid, or by cluster when cluster is given)
boot_mean <- function(x, cluster = NULL, R = 2000, seed = 1) {
  set.seed(seed)
  if (is.null(cluster)) {
    b <- replicate(R, mean(x[sample.int(length(x), replace = TRUE)]))
  } else {
    cl <- split(x, cluster); s <- sapply(cl, sum); n <- sapply(cl, length)
    b <- replicate(R, { k <- sample.int(length(cl), replace = TRUE); sum(s[k]) / sum(n[k]) })
  }
  c(mean = mean(x), lo = unname(quantile(b, 0.025)), hi = unname(quantile(b, 0.975)))
}

# Paired comparison of log loss: mean of (loss_1 - loss_2) with a bootstrap CI.
# Negative means model 1 is better.
compare_logloss <- function(p1, p2, y, cluster = NULL) {
  ok <- !is.na(y) & !is.na(p1) & !is.na(p2)
  d <- logloss_vec(p1[ok], y[ok]) - logloss_vec(p2[ok], y[ok])
  boot_mean(d, if (is.null(cluster)) NULL else cluster[ok])
}

# Flat-stake value betting at real odds: on each game, bet 1 unit on the side
# whose expected value under the model, p * odds - 1, is largest, if it is above
# `threshold`. A draw/tie (y = NA) is a push: the stake is returned.
bet_backtest <- function(p, y, oA, oB, threshold = 0) {
  ok <- !is.na(p) & !is.na(oA) & !is.na(oB)
  evA <- p * oA - 1; evB <- (1 - p) * oB - 1
  side <- ifelse(evA >= evB, "A", "B"); ev <- pmax(evA, evB)
  bet <- ok & ev > threshold
  profit <- ifelse(is.na(y), 0,
                   ifelse(side == "A", ifelse(y == 1, oA - 1, -1), ifelse(y == 0, oB - 1, -1)))
  pr <- profit[bet]
  roi <- if (length(pr)) boot_mean(pr) else c(mean = NA, lo = NA, hi = NA)
  list(n_games = sum(ok), n_bets = sum(bet), roi = roi, profit = sum(pr),
       cum = cumsum(ifelse(bet, profit, 0)[ok]), avg_margin = mean((1 / oA + 1 / oB - 1)[ok]))
}

# Rock-paper-scissors cycles implied by the fitted PELO state at a point in time:
# among triads where every pair has met at least `min_meet` times, count those
# whose neutral-venue win probabilities form a cycle (a > b, b > c, c > a).
count_cycles <- function(state, players, meet, min_meet = 2) {
  r <- state$r; B <- state$B; n <- length(players)
  P <- 1 / (1 + 10^(-(outer(r, r, "-") + B) / 400))
  ok <- meet >= min_meet
  cand <- which(rowSums(ok) >= 2)
  n_tri <- 0; cyc <- list()
  for (a in cand) for (b in cand[cand > a]) if (ok[a, b]) for (c in cand[cand > b]) if (ok[a, c] && ok[b, c]) {
    n_tri <- n_tri + 1
    ab <- P[a, b] > 0.5; bc <- P[b, c] > 0.5; ca <- P[c, a] > 0.5
    if ((ab && bc && ca) || (!ab && !bc && !ca)) {
      o <- if (ab) c(a, b, c) else c(a, c, b)
      cyc[[length(cyc) + 1]] <- data.frame(x = players[o[1]], y = players[o[2]], z = players[o[3]],
        p_xy = P[o[1], o[2]], p_yz = P[o[2], o[3]], p_zx = P[o[3], o[1]],
        strength = min(P[o[1], o[2]], P[o[2], o[3]], P[o[3], o[1]]))
    }
  }
  cyc <- if (length(cyc)) do.call(rbind, cyc) else NULL
  if (!is.null(cyc)) cyc <- cyc[order(-cyc$strength), ]
  list(n_triads = n_tri, n_cycles = if (is.null(cyc)) 0 else nrow(cyc), cycles = cyc)
}

# Number of earlier meetings of each game's pair (in either orientation)
prior_meetings <- function(i, j) {
  key <- paste(pmin(i, j), pmax(i, j)); ave(seq_along(key), key, FUN = seq_along) - 1
}
