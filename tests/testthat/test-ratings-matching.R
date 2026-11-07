# Ratings must be matched to players by name, never by row position.

make_data <- function(ids, n = 80, seed = 1) {
  set.seed(seed)
  m <- t(replicate(n, sample(ids, 2)))
  data.frame(Time = seq_len(n), A = m[, 1], B = m[, 2], match_result = rbinom(n, 1, 0.5),
             stringsAsFactors = FALSE)
}
outcome_nll <- function(preds, y) -sum(log(pmax(ifelse(y == 1, preds, 1 - preds), 1e-6)))

ids <- c("Ann", "Bob", "Cat", "Dan")
d   <- make_data(ids)
r_sorted   <- data.frame(Player = sort(ids), Ratings = c(1600, 1500, 1450, 1400))
r_shuffled <- r_sorted[c(3, 1, 4, 2), ]
r_extra    <- rbind(r_shuffled, data.frame(Player = c("Eve", "Fay"), Ratings = c(1700, 1300)))
adv <- initialize_pair_adv_mat(sort(ids), 0)

test_that("row order of current_ratings does not change the likelihood", {
  expect_identical(get_negloglike_elo(30, d, r_shuffled), get_negloglike_elo(30, d, r_sorted))
  expect_identical(get_negloglike_pelo(c(30, 20), d, r_shuffled, adv),
                   get_negloglike_pelo(c(30, 20), d, r_sorted, adv))
})

test_that("extra players in current_ratings are ignored", {
  expect_identical(get_negloglike_elo(30, d, r_extra), get_negloglike_elo(30, d, r_sorted))
  adv_extra <- initialize_pair_adv_mat(sort(c(ids, "Eve", "Fay")), 0)
  expect_identical(get_negloglike_pelo(c(30, 20), d, r_extra, adv_extra),
                   get_negloglike_pelo(c(30, 20), d, r_sorted, adv))
})

test_that("a player missing from current_ratings is an error", {
  expect_error(get_negloglike_elo(30, d, r_sorted[-2, ]), "not found in current_ratings")
  expect_error(get_negloglike_pelo(c(30, 20), d, r_sorted[-2, ], adv), "not found in current_ratings")
})

test_that("likelihood functions agree with run_elo / run_pelo predictions", {
  idm <- get_player_id_mapping(d)
  pe <- run_pelo(c(30, 20), d, r_shuffled, adv, idm)
  expect_equal(get_negloglike_pelo(c(30, 20), d, r_shuffled, adv), outcome_nll(pe$preds, d$match_result))
  el <- run_elo(30, d, r_shuffled, idm)
  expect_equal(get_negloglike_elo(30, d, r_shuffled), outcome_nll(el$preds, d$match_result))
})

test_that("numeric player IDs work and match the same players given as text", {
  num_ids <- c(101L, 205L, 330L, 999L)
  dn <- make_data(num_ids, seed = 2)
  dc <- dn; dc$A <- paste0("p", dn$A); dc$B <- paste0("p", dn$B)
  rn <- initialize_ratings(sort(num_ids), 1500); an <- initialize_pair_adv_mat(sort(num_ids), 0)
  rc <- initialize_ratings(sort(unique(c(dc$A, dc$B))), 1500); ac <- initialize_pair_adv_mat(rc$Player, 0)
  expect_equal(get_negloglike_pelo(c(30, 20), dn, rn, an), get_negloglike_pelo(c(30, 20), dc, rc, ac))
  expect_equal(get_negloglike_elo(30, dn, rn), get_negloglike_elo(30, dc, rc))
  expect_equal(run_pelo(c(30, 20), dn, rn, an, get_player_id_mapping(dn))$preds,
               run_pelo(c(30, 20), dc, rc, ac, get_player_id_mapping(dc))$preds)
})
