# Check the engine against the PELO package (github.com/kh-w/PELO) on the tennis data.
# Without surface ratings the engine must reproduce PELO::get_negloglike_pelo,
# PELO::get_negloglike_elo and PELO::run_pelo exactly. With the surface rate Ks = 0
# the surface code path must give the same results as the plain one.
#
# Run from the repository root:  Rscript tests/test_engine.R
suppressMessages(library(PELO))
source("R/pelo_engine.R")

g  <- read.csv("data/tennis_atp_tour_games.csv", stringsAsFactors = FALSE)
ix <- index_players(g)
pk <- data.frame(Time = seq_len(nrow(g)), A = g$A, B = g$B, match_result = g$y)
r0 <- initialize_ratings(ix$players, 1500); a0 <- initialize_pair_adv_mat(ix$players, 0)
sf <- list(s = match(g$surface, sort(unique(g$surface))), ns = length(unique(g$surface)))

ok <- TRUE
for (KK in list(c(25, 0), c(25, 12), c(7.3, 55.1))) {
  mine <- pelo_pass(c(K = KK[1], K2 = KK[2], H = 0), ix$i, ix$j, g$y, g$h, ix$np)
  d1 <- abs(nll(mine$p, g$y) - get_negloglike_pelo(KK, pk, r0, a0))
  d2 <- if (KK[2] == 0) abs(nll(mine$p, g$y) - get_negloglike_elo(KK[1], pk, r0)) else 0
  rp <- run_pelo(KK, pk, r0, a0, get_player_id_mapping(pk))
  d3 <- max(abs(mine$p - rp$preds))
  d4 <- max(abs(mine$state$r - rp$ratings$Ratings[match(as.character(ix$players), as.character(rp$ratings$Player))]))
  pn <- as.character(ix$players)
  d5 <- max(abs(mine$state$B - rp$pair_adv[pn, pn]))
  s0 <- pelo_pass(c(K = KK[1], K2 = KK[2], H = 0, Ks = 1e-300), ix$i, ix$j, g$y, g$h, ix$np, s = sf$s, ns = sf$ns)
  d6 <- max(abs(s0$p - mine$p))
  cat(sprintf("K=%5.1f K2=%5.1f  |NLL diff| pelo %.1e elo %.1e | preds %.1e | ratings %.1e | pair adv %.1e | surface path %.1e\n",
              KK[1], KK[2], d1, d2, d3, d4, d5, d6))
  ok <- ok && max(d1, d2, d3, d4, d5, d6) < 1e-9
}
cat(if (ok) "ALL ENGINE CHECKS PASSED\n" else "ENGINE CHECK FAILED\n")
if (!ok) quit(status = 1)
