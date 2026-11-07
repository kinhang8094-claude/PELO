# Pairwise advantages are stored as a compact pair table unless a matrix is asked for.

make_data <- function(ids, n = 120, seed = 1) {
  set.seed(seed)
  m <- t(replicate(n, sample(ids, 2)))
  data.frame(Time = seq_len(n), A = m[, 1], B = m[, 2], match_result = rbinom(n, 1, 0.5),
             stringsAsFactors = FALSE)
}
ids <- c("Ann", "Bob", "Cat", "Dan", "Eve")
d   <- make_data(ids)
r0  <- initialize_ratings(sort(ids), 1500)
idm <- get_player_id_mapping(d)
a0  <- initialize_pair_adv_mat(sort(ids), 0)
a1  <- a0; a1["Ann", "Cat"] <- 25; a1["Cat", "Ann"] <- -25

test_that("run_pelo returns a pair table by default and a matrix on request", {
  tb <- run_pelo(c(30, 20), d, r0, a1, idm)
  mx <- run_pelo(c(30, 20), d, r0, a1, idm, pair_adv_format = "matrix")
  expect_s3_class(tb$pair_adv, "data.frame")
  expect_named(tb$pair_adv, c("Player1", "Player2", "PairAdv"))
  expect_true(is.matrix(mx$pair_adv))
  expect_identical(tb$preds, mx$preds)
  expect_identical(as_pair_adv_matrix(tb$pair_adv, rownames(mx$pair_adv)), mx$pair_adv)
})

test_that("NULL, a zero matrix and a pair table give the same results", {
  expect_identical(get_negloglike_pelo(c(30, 20), d, r0), get_negloglike_pelo(c(30, 20), d, r0, a0))
  expect_identical(run_pelo(c(30, 20), d, r0, NULL, idm)$preds, run_pelo(c(30, 20), d, r0, a0, idm)$preds)
  expect_identical(get_negloglike_pelo(c(30, 20), d, r0, as_pair_adv_table(a1)),
                   get_negloglike_pelo(c(30, 20), d, r0, a1))
})

test_that("a run can continue from the pair table it returned", {
  first <- run_pelo(c(30, 20), d[1:60, ], r0, a1, idm)
  second <- run_pelo(c(30, 20), d[61:120, ], first$ratings, first$pair_adv, first$player_id_mapping)
  whole <- run_pelo(c(30, 20), d, r0, a1, idm)
  expect_identical(c(first$preds, second$preds), whole$preds)
})

test_that("advantages of pairs that don't meet are kept", {
  d2 <- d[!(d$A %in% c("Ann", "Cat") & d$B %in% c("Ann", "Cat")), ]
  out <- run_pelo(c(30, 20), d2, r0, a1, get_player_id_mapping(d2))$pair_adv
  expect_equal(out$PairAdv[out$Player1 == "Ann" & out$Player2 == "Cat"], 25)
})

test_that("history and historical_win_prob work with pair tables", {
  ht <- run_pelo(c(30, 20), d[1:30, ], r0, a1, idm, keep_history = TRUE)
  hm <- run_pelo(c(30, 20), d[1:30, ], r0, a1, idm, keep_history = TRUE, pair_adv_format = "matrix")
  expect_s3_class(ht$pair_adv_hist[[2]], "data.frame")
  expect_identical(historical_win_prob(ht$ratings_hist, ht$pair_adv_hist),
                   historical_win_prob(hm$ratings_hist, hm$pair_adv_hist))
})

test_that("converters round-trip and invalid tables are rejected", {
  expect_identical(as_pair_adv_matrix(as_pair_adv_table(a1), sort(ids)), a1)
  bad <- data.frame(Player1 = c("Ann", "Bob"), Player2 = c("Bob", "Ann"), PairAdv = c(5, 5))
  expect_error(get_negloglike_pelo(c(1, 1), d, r0, bad), "not negative")
  expect_error(get_negloglike_pelo(c(1, 1), d, r0, data.frame(x = 1)), "Player1")
})

test_that("many players need no players x players matrix", {
  big <- make_data(sprintf("P%05d", 1:20000), n = 3000, seed = 3)
  pl <- sort(unique(c(big$A, big$B)))
  out <- run_pelo(c(30, 20), big, initialize_ratings(pl, 1500), NULL, get_player_id_mapping(big))
  expect_lte(nrow(out$pair_adv), nrow(big))
  expect_lt(as.numeric(object.size(out$pair_adv)), 1e6)
})
