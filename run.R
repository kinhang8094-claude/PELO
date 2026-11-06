# ATP tour-level singles 1991-2024 - do matchup effects (intransitivity) exist,
# and does PELO capture them? Run from the repository root.
#
# Models (all fitted by maximum likelihood on the first 80% of matches):
#   Elo        overall rating                         (K)
#   PELO       Elo + pairwise advantage               (K, K2)
#   sElo       Elo + one extra rating per surface     (K, Ks)
#   sPELO      sElo + pairwise advantage              (K, Ks, K2)
# The surface models check that an apparent "matchup" is not just a surface effect.

source("R/pelo_engine.R"); source("R/evaluate.R")
suppressMessages(library(jsonlite))
set.seed(1)
out_dir <- "results"; dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

g <- read.csv("data/tennis_atp_tour_games.csv", stringsAsFactors = FALSE)
g$date <- as.Date(g$date); g$period <- seq_len(nrow(g))
ix <- index_players(g)
names_of <- tapply(c(g$A_name, g$B_name), c(g$A, g$B), function(v) tail(v, 1))
lab <- function(id) as.vector(names_of[as.character(id)])
surf <- list(s = match(g$surface, sort(unique(g$surface))), ns = length(unique(g$surface)))
n <- nrow(g); train <- seq_len(n) <= floor(0.8 * n); test <- !train
y <- g$y; yt <- ifelse(test, y, NA)

# ---------------------------------------------------------------------------
# 1. Fit the four models and predict walk-forward
# ---------------------------------------------------------------------------
fits <- list(
  Elo   = fit_model("elo",  g, ix, train),
  PELO  = fit_model("pelo", g, ix, train),
  sElo  = fit_model("elo",  g, ix, train, surface = surf),
  sPELO = fit_model("pelo", g, ix, train, surface = surf))
pr <- lapply(names(fits), function(m) predict_all(fits[[m]], g, ix, surface = if (grepl("^s", m)) surf))
names(pr) <- names(fits)
P <- sapply(pr, `[[`, "p")
nobeta <- function(o) 1 / (1 + 10^(-o$z / 400))          # same ratings, pairwise term removed

pm <- prior_meetings(ix$i, ix$j)
rep3 <- test & pm >= 3
res <- list(
  data = list(matches = n, players = ix$np, first = as.character(min(g$date)), last = as.character(max(g$date)),
              train = sum(train), test = sum(test), test_start = as.character(min(g$date[test])),
              test_share_met_before = mean(pm[test] >= 1), test_share_met_3plus = mean(pm[test] >= 3),
              test_share_met_5plus = mean(pm[test] >= 5)),
  fit = lapply(fits, function(f) c(as.list(f$par), nll_train = f$nll, nll_per_match = f$nll / f$n_train)),
  test = lapply(as.data.frame(P), function(p) metrics(p[test], y[test])),
  test_repeat3 = lapply(as.data.frame(P), function(p) metrics(p[rep3], y[rep3])),
  pelo_vs_elo   = compare_logloss(P[, "PELO"],  P[, "Elo"],  yt),
  spelo_vs_selo = compare_logloss(P[, "sPELO"], P[, "sElo"], yt),
  selo_vs_elo   = compare_logloss(P[, "sElo"],  P[, "Elo"],  yt),
  pelo_vs_elo_repeat3   = compare_logloss(P[, "PELO"],  P[, "Elo"],  ifelse(rep3, y, NA)),
  spelo_vs_selo_repeat3 = compare_logloss(P[, "sPELO"], P[, "sElo"], ifelse(rep3, y, NA)),
  beta_alone = list(
    PELO  = compare_logloss(P[, "PELO"],  nobeta(pr$PELO),  yt),
    sPELO = compare_logloss(P[, "sPELO"], nobeta(pr$sPELO), yt),
    PELO_repeat3  = compare_logloss(P[, "PELO"],  nobeta(pr$PELO),  ifelse(rep3, y, NA)),
    sPELO_repeat3 = compare_logloss(P[, "sPELO"], nobeta(pr$sPELO), ifelse(rep3, y, NA))))

# ---------------------------------------------------------------------------
# 2. K2 profiles (training NLL relative to K2 = 0, other parameters refitted)
# ---------------------------------------------------------------------------
res$profile_K2 <- list(PELO = profile_K2(g, ix, train, "sequential", FALSE),
                       sPELO = profile_K2(g, ix, train, "sequential", FALSE, surface = surf))

# ---------------------------------------------------------------------------
# 3. Model-free test for persistent matchup effects.
#    Residual = result minus a (recalibrated) rating-model prediction. If player X
#    really has an edge over player Y beyond their ratings, X's surprise wins over Y
#    in their early meetings should continue in their later meetings.
#    For every pair with at least `min_n` meetings, compare the mean residual of the
#    first half of their meetings with that of the second half. The null distribution
#    comes from re-pairing first and second halves at random (permutation).
# ---------------------------------------------------------------------------
persistence_test <- function(p, min_n = 4, R = 5000) {
  cal <- glm(y ~ qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6)), family = binomial)    # recalibrate on all matches
  q <- fitted(cal); e <- y - q
  key <- paste(pmin(ix$i, ix$j), pmax(ix$i, ix$j))
  sgn <- ifelse(ix$i < ix$j, 1, -1)                     # residual from the lower-id player's side
  d <- split(data.frame(e = e * sgn, v = q * (1 - q)), key)
  d <- d[sapply(d, nrow) >= min_n]
  h <- t(sapply(d, function(x) { k <- floor(nrow(x) / 2)
    c(m1 = mean(x$e[1:k]), m2 = mean(x$e[(nrow(x) - k + 1):nrow(x)]), n = nrow(x),
      v1 = sum(x$v[1:k]) / k^2, v2 = sum(x$v[(nrow(x) - k + 1):nrow(x)]) / k^2) }))
  w <- 1 / sqrt(h[, "v1"] * h[, "v2"])                   # precision weights
  stat <- function(m2) sum(w * h[, "m1"] * m2) / sum(w)
  obs <- stat(h[, "m2"]); null <- replicate(R, stat(sample(h[, "m2"])))
  # Implied spread of true pairwise effects: E[m1*m2] = Var(effect) on the probability scale
  list(pairs = nrow(h), matches = sum(h[, "n"]), cov = obs, p_value = mean(null >= obs),
       null_sd = sd(null), z = (obs - mean(null)) / sd(null),
       implied_sd_prob = if (obs > 0) sqrt(obs) else 0,
       corr = cov.wt(cbind(h[, "m1"], h[, "m2"]), wt = w / sum(w), cor = TRUE)$cor[1, 2])
}
# Player-adjusted version: remove each player's general form before testing pairs.
# For a match between X and Y in year t, subtract X's mean surprise in year t against
# everyone except Y, and add Y's mean surprise in year t against everyone except X.
# What remains can't be explained by a player being under- or over-rated that year.
persistence_test_adj <- function(p, min_n = 4, R = 5000) {
  cal <- glm(y ~ qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6)), family = binomial)
  e <- y - fitted(cal); yr <- format(g$date, "%Y")
  pid <- c(ix$i, ix$j); opp <- c(ix$j, ix$i); se <- c(e, -e); yy <- c(yr, yr)
  tot <- tapply(se, paste(pid, yy), sum); cnt <- tapply(se, paste(pid, yy), length)
  ptot <- tapply(se, paste(pid, opp, yy), sum); pcnt <- tapply(se, paste(pid, opp, yy), length)
  form <- function(pl, op) { k1 <- paste(pl, yr); k2 <- paste(pl, op, yr)
    num <- tot[k1] - ptot[k2]; den <- cnt[k1] - pcnt[k2]; ifelse(den > 0, num / den, 0) }
  e_adj <- e - form(ix$i, ix$j) + form(ix$j, ix$i)
  key <- paste(pmin(ix$i, ix$j), pmax(ix$i, ix$j)); sgn <- ifelse(ix$i < ix$j, 1, -1)
  d <- split(e_adj * sgn, key); d <- d[sapply(d, length) >= min_n]
  h <- t(sapply(d, function(x) { k <- floor(length(x) / 2)
    c(m1 = mean(x[1:k]), m2 = mean(x[(length(x) - k + 1):length(x)]), n = length(x), k = k) }))
  w <- h[, "k"]
  stat <- function(m2) sum(w * h[, "m1"] * m2) / sum(w)
  obs <- stat(h[, "m2"]); null <- replicate(R, stat(sample(h[, "m2"])))
  list(pairs = nrow(h), cov = obs, p_value = mean(null >= obs), z = (obs - mean(null)) / sd(null),
       implied_sd_prob = if (obs > 0) sqrt(obs) else 0)
}
res$persistence_player_adjusted <- list(sElo = persistence_test_adj(P[, "sElo"]),
                                        sElo_min8 = persistence_test_adj(P[, "sElo"], min_n = 8))

res$persistence <- list(Elo  = persistence_test(P[, "Elo"]),
                        sElo = persistence_test(P[, "sElo"]),
                        sElo_min8 = persistence_test(P[, "sElo"], min_n = 8))

# ---------------------------------------------------------------------------
# 4. Cycles implied by the fitted sPELO at the end of training (neutral, surface-free part)
#    and how the cyclic matchups went afterwards.
# ---------------------------------------------------------------------------
trd <- train & !is.na(y)
tab <- table(factor(ix$i[trd], 1:ix$np), factor(ix$j[trd], 1:ix$np)); meet <- unclass(tab) + t(unclass(tab))
st <- pelo_pass(fits$sPELO$par, ix$i[train], ix$j[train], y[train], g$h[train], ix$np,
                s = surf$s[train], ns = surf$ns)$state
cyc <- count_cycles(st, ix$players, meet, min_meet = 3)
if (!is.null(cyc$cycles)) for (v in c("x", "y", "z")) cyc$cycles[[v]] <- lab(cyc$cycles[[v]])
res$cycles <- list(n_triads = cyc$n_triads, n_cycles = cyc$n_cycles,
                   share = cyc$n_cycles / max(cyc$n_triads, 1),
                   top = if (!is.null(cyc$cycles)) head(cyc$cycles, 10))

# Largest pairwise terms at the end of the data among frequent rivals
st_end <- pr$sPELO$state
keyall <- paste(pmin(ix$i, ix$j), pmax(ix$i, ix$j)); cnt <- table(keyall)
big <- names(cnt)[cnt >= 15]
bt <- do.call(rbind, lapply(big, function(k) { ab <- as.integer(strsplit(k, " ")[[1]])
  data.frame(X = lab(ix$players[ab[1]]), Y = lab(ix$players[ab[2]]), meetings = as.integer(cnt[k]),
             beta_XY = st_end$B[ab[1], ab[2]]) }))
bt$X2 <- ifelse(bt$beta_XY >= 0, bt$X, bt$Y); bt$Y2 <- ifelse(bt$beta_XY >= 0, bt$Y, bt$X)
bt <- data.frame(favoured = bt$X2, over = bt$Y2, meetings = bt$meetings, beta = abs(bt$beta_XY))
res$largest_pairwise <- head(bt[order(-bt$beta), ], 12)

# ---------------------------------------------------------------------------
# 5. Market comparison and value betting on Grand Slam test matches (Pinnacle closing odds)
# ---------------------------------------------------------------------------
ot <- test & !is.na(g$mktA)
res$market <- list(n = sum(ot), market = metrics(g$mktA[ot], y[ot]),
                   models = lapply(as.data.frame(P), function(p) metrics(p[ot], y[ot])))
res$betting <- lapply(as.data.frame(P), function(p) {
  b <- bet_backtest(p[ot], y[ot], g$oddsA[ot], g$oddsB[ot], 0)
  list(n_bets = b$n_bets, roi = b$roi, avg_margin = b$avg_margin) })

saveRDS(res, file.path(out_dir, "summary.rds"))
write_json(res, file.path(out_dir, "summary.json"), auto_unbox = TRUE, digits = 8, pretty = TRUE)
write.csv(data.frame(date = g$date, A = lab(g$A), B = lab(g$B), surface = g$surface, y = y, test = test,
                     prior_meetings = pm, round(P, 6), mktA = g$mktA),
          file.path(out_dir, "predictions.csv"), row.names = FALSE)

# Figure: first-half vs second-half residuals of frequent pairs
png(file.path(out_dir, "matchup_persistence.png"), width = 1600, height = 1200, res = 200)
cal <- glm(y ~ qlogis(pmin(pmax(P[, "sElo"], 1e-6), 1 - 1e-6)), family = binomial); e <- y - fitted(cal)
key <- paste(pmin(ix$i, ix$j), pmax(ix$i, ix$j)); sg <- ifelse(ix$i < ix$j, 1, -1)
d <- split(e * sg, key); d <- d[sapply(d, length) >= 8]
hh <- t(sapply(d, function(x) { k <- floor(length(x) / 2); c(mean(x[1:k]), mean(x[(length(x) - k + 1):length(x)])) }))
plot(hh, pch = 16, col = adjustcolor("#1f6fb4", 0.5), xlab = "Mean surprise, first half of the pair's meetings",
     ylab = "Mean surprise, second half", main = "ATP rivalries with 8+ meetings: does a matchup edge persist?")
abline(h = 0, v = 0, col = "grey70"); abline(lm(hh[, 2] ~ hh[, 1]), col = "#c0392b", lwd = 2)
mtext("Surprise = result minus surface-Elo prediction (recalibrated), from one player's side", cex = 0.75)
dev.off()
cat("done\n")
