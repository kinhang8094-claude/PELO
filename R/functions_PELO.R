library(compiler)

#' @importFrom stats runif setNames
NULL

#' Prepare a Match Dataframe for Elo/PELO Calculations (internal)
#'
#' This function standardizes a dataframe for Elo or PELO calculations by
#' ensuring it has the correct column names and initializing match outcomes as NA.
#' It is intended for internal use only.
#'
#' @param df Data frame. Must have exactly 4 columns corresponding to Time, Player A, Player B, and match result.
#' @return Data frame. Same as input but with column names standardized and `match_result` initialized to `NA_integer_`.
#' @keywords internal
process_dataframe <- function(df) {
  if (ncol(df) != 4) stop("Input dataframe must have exactly 4 columns.")
  colnames(df) <- c("Time", "A", "B", "match_result")
  df$match_result <- NA_integer_  # use integer NA for efficiency
  return(df)
}

#' Generate Player ID Mapping from the given dataset
#'
#' This function extracts all unique players from the match dataframe and assigns
#' a numeric ID to each player. The dataframe must have columns named
#' `"Time"`, `"A"`, `"B"`, and `"match_result"`.
#'
#' @param df A dataframe containing match information. Must have exactly four columns:
#'           `Time`, `A`, `B`, and `match_result`.
#'
#' @return A dataframe with two columns:
#' \describe{
#'   \item{Names}{Character vector of unique player names sorted alphabetically.}
#'   \item{ID}{Integer IDs assigned sequentially starting from 1.}
#' }
#'
#' @examples
#' df <- data.frame(
#'   Time = 1:3,
#'   A = c("Alice", "Bob", "Charlie"),
#'   B = c("Bob", "Charlie", "Alice"),
#'   match_result = c(1, 0, 1)
#' )
#' player_mapping <- get_player_id_mapping(df)
#' print(player_mapping)
#'
#' @export
get_player_id_mapping <- function(df){
  if(!all(colnames(df) == c("Time", "A", "B", "match_result"))){
    stop("Use colnames c('Time','A','B','match_result') for your dataset.")
  }
  players <- sort(unique(c(df$A, df$B)))
  return(data.frame(Names = players, ID = seq_along(players)))
}

#' Initialize Ratings for Players
#'
#' Creates a dataframe of players with an initial rating value.
#'
#' @param players Character vector of player names.
#' @param initial_rating_val Numeric value for the initial rating of each player.
#'
#' @return A dataframe with columns:
#' \describe{
#'   \item{Player}{Player names.}
#'   \item{Ratings}{Initial rating values for each player.}
#' }
#'
#' @examples
#' initialize_ratings(c("Alice", "Bob"), 1500)
#'
#' @export
initialize_ratings <- function(players, initial_rating_val){
  data.frame(Player = players, Ratings = initial_rating_val)
}

#' Initialize Pairwise Advantage Matrix
#'
#' Creates a square matrix of pairwise advantages between players.
#'
#' @param players Character vector of player names.
#' @param initial_pair_adv_mat_val Numeric value to initialize all pairwise advantages.
#'
#' @return A numeric matrix with row and column names equal to player names.
#'
#' @examples
#' initialize_pair_adv_mat(c("Alice", "Bob"), 0)
#'
#' @export
initialize_pair_adv_mat <- function(players, initial_pair_adv_mat_val){
  matrix(initial_pair_adv_mat_val, nrow = length(players), ncol = length(players),
         dimnames = list(players, players))
}

#' Calculate Elo Winning Probability
#'
#' Computes the probability that player 1 wins against player 2 using Elo ratings.
#'
#' @param p1_r Numeric, rating of player 1.
#' @param p2_r Numeric, rating of player 2.
#'
#' @return Numeric value between 0 and 1 representing the probability of player 1 winning.
#'
#' @examples
#' winning_prob_elo(1600, 1500)
#'
#' @export
winning_prob_elo <- function(p1_r, p2_r){
  1 / (1 + 10^(-(p1_r - p2_r)/400))
}

#' Calculate PELO Winning Probability with Pairwise Advantage
#'
#' Computes the probability that player 1 wins against player 2 considering a pairwise advantage.
#'
#' @param p1_r Numeric, rating of player 1.
#' @param p2_r Numeric, rating of player 2.
#' @param p1_adv_p2 Numeric, pairwise advantage of player 1 over player 2.
#'
#' @return Numeric value between 0 and 1 representing the probability of player 1 winning.
#'
#' @examples
#' winning_prob_pelo(1600, 1500, 50)
#'
#' @export
winning_prob_pelo <- function(p1_r, p2_r, p1_adv_p2){
  1 / (1 + 10^(-(p1_r + p1_adv_p2 - p2_r)/400))
}

#' Simulate an Elo game outcome (internal)
#'
#' Simulate the outcome of a match between two players using Elo ratings.
#' This function is intended for internal use only.
#'
#' @param p1_r Numeric. Rating of player 1.
#' @param p2_r Numeric. Rating of player 2.
#' @return Integer. 1 if player 1 wins, 0 if player 2 wins.
#' @keywords internal
game_outcome_elo <- function(p1_r, p2_r) {
  p <- winning_prob_elo(p1_r, p2_r)
  return(as.integer(stats::runif(1) < p))
}

#' Simulate a PELO game outcome (internal)
#'
#' Simulate the outcome of a match between two players using the PELO model.
#' This function is intended for internal use only.
#'
#' @param p1_r Numeric. Rating of player 1.
#' @param p2_r Numeric. Rating of player 2.
#' @param p1_adv_p2 Numeric. Pairwise advantage of player 1 over player 2.
#' @return Integer. 1 if player 1 wins, 0 if player 2 wins.
#' @keywords internal
game_outcome_pelo <- function(p1_r, p2_r, p1_adv_p2) {
  p <- winning_prob_pelo(p1_r, p2_r, p1_adv_p2)
  return(as.integer(stats::runif(1) < p))
}

#' Update Elo Ratings After a Match
#'
#' Updates the Elo ratings of two players based on the match outcome.
#'
#' @param p1_r Numeric, current rating of player 1.
#' @param p2_r Numeric, current rating of player 2.
#' @param K Numeric, Elo K-factor.
#' @param X Integer, match outcome (1 if player 1 wins, 0 if player 2 wins).
#'
#' @return Numeric vector of updated ratings: c(new_p1_r, new_p2_r)
#'
#' @examples
#' ratings_update_elo(1600, 1500, K = 32, X = 1)
#'
#' @export
ratings_update_elo <- function(p1_r, p2_r, K, X) {
  expected <- winning_prob_elo(p1_r, p2_r)
  p1_r_new <- p1_r + K * (X - expected)
  p2_r_new <- p2_r - K * (X - expected)
  c(p1_r_new, p2_r_new)
}

#' Update PELO Ratings After a Match
#'
#' Updates the PELO ratings of two players based on match outcome and pairwise advantage.
#'
#' @param p1_r Numeric, current rating of player 1.
#' @param p2_r Numeric, current rating of player 2.
#' @param p1_adv_p2 Numeric, pairwise advantage of player 1 over player 2.
#' @param K Numeric, PELO K-factor for ratings.
#' @param X Integer, match outcome (1 if player 1 wins, 0 if player 2 wins).
#'
#' @return Numeric vector of updated ratings: c(new_p1_r, new_p2_r)
#'
#' @examples
#' ratings_update_pelo(1600, 1500, 50, K = 32, X = 1)
#'
#' @export
ratings_update_pelo <- function(p1_r, p2_r, p1_adv_p2, K, X) {
  expected <- winning_prob_pelo(p1_r, p2_r, p1_adv_p2)
  p1_r_new <- p1_r + K * (X - expected)
  p2_r_new <- p2_r - K * (X - expected)
  c(p1_r_new, p2_r_new)
}

#' Update Pairwise Advantage in PELO
#'
#' Updates the pairwise advantage matrix entries for a match between two players.
#'
#' @param p1_r Numeric, rating of player 1.
#' @param p2_r Numeric, rating of player 2.
#' @param p1_adv_p2 Numeric, current advantage of player 1 over player 2.
#' @param p2_adv_p1 Numeric, current advantage of player 2 over player 1 (should be negative of p1_adv_p2).
#' @param K2 Numeric, P-Elo K2-factor for pairwise advantage updates.
#' @param X Integer, match outcome (1 if player 1 wins, 0 if player 2 wins).
#'
#' @return Numeric vector of updated pairwise advantages: c(new_p1_adv_p2, new_p2_adv_p1)
#'
#' @examples
#' pairadv_update_pelo(1600, 1500, 50, -50, K2 = 16, X = 1)
#'
#' @export
pairadv_update_pelo <- function(p1_r, p2_r, p1_adv_p2, p2_adv_p1, K2, X){
  if(p1_adv_p2 + p2_adv_p1 != 0){
    stop("p1_adv_p2 is not negative p2_adv_p1.")
  }
  expected <- winning_prob_pelo(p1_r, p2_r, p1_adv_p2)
  p1_adv_p2_new <- p1_adv_p2 + K2 * (X - expected)
  p2_adv_p1_new <- p2_adv_p1 - K2 * (X - expected)
  if(p1_adv_p2_new + p2_adv_p1_new != 0){
    stop("Error: p1_adv_p2_new is not negative p2_adv_p1_new.")
  }
  c(p1_adv_p2_new, p2_adv_p1_new)
}

#' Linear indices of pairwise-advantage entries (internal)
#'
#' For every match, returns the linear indices of `pair_adv_mat[A, B]` and
#' `pair_adv_mat[B, A]`, so the match loops can read and write the matrix
#' without name lookups. Also performs, on the starting values, the same
#' antisymmetry check as `pairadv_update_pelo()`.
#'
#' @param pair_adv_mat Numeric matrix with player names as row and column names.
#' @param A,B Character vectors of player names (first and second player of each match).
#' @return List with integer vectors `ab` and `ba`.
#' @keywords internal
#' @noRd
.pair_adv_lin_index <- function(pair_adv_mat, A, B) {
  ra <- match(A, rownames(pair_adv_mat)); rb <- match(B, rownames(pair_adv_mat))
  ca <- match(A, colnames(pair_adv_mat)); cb <- match(B, colnames(pair_adv_mat))
  if (anyNA(ra) || anyNA(rb) || anyNA(ca) || anyNA(cb)) stop("subscript out of bounds")
  nr <- nrow(pair_adv_mat)
  ab <- ra + (cb - 1L) * nr
  ba <- rb + (ca - 1L) * nr
  if (any(pair_adv_mat[ab] + pair_adv_mat[ba] != 0)) {
    stop("p1_adv_p2 is not negative p2_adv_p1.")
  }
  list(ab = ab, ba = ba)
}

#' Negative Log-Likelihood for Elo Ratings
#'
#' Computes the negative log-likelihood of a set of Elo ratings given observed match outcomes.
#' This function is intended for use in optimization to estimate the best Elo K-factor.
#'
#' @param K_guess Numeric, the Elo K-factor to evaluate.
#' @param data A dataframe of match results with columns: `Time`, `A`, `B`, `match_result`.
#'             `match_result` must be binary: 1 if player A won, 0 if player B won.
#' @param current_ratings A dataframe with columns `Player` and `Ratings` containing
#'                        the current ratings of all players.
#'
#' @return Numeric, the negative log-likelihood of the match outcomes given the current ratings
#'         and the K-factor.
#'
#' @details
#' The function loops over all matches, computes the predicted probability of the observed
#' outcome based on the Elo formula, updates player ratings sequentially, and accumulates
#' the log-likelihood. A minimum probability of 1e-6 is used to prevent numerical issues
#' with log(0).
#'
#' @examples
#' # Suppose df contains match data and ratings contains current ratings
#' # get_negloglike_elo(32, df, ratings)
#'
#' @export
get_negloglike_elo <- cmpfun(function(K_guess, data, current_ratings){

  df <- data

  # Ensure the dataframe has the correct column names
  if(!all(colnames(df) == c("Time", "A", "B", "match_result"))){
    stop("Use colnames c('Time','A','B','match_result') for your dataset.")
  }
  # Ensure match outcomes are binary win/lose
  if (!all(df$match_result %in% c(0, 1))) {
    stop("The column 'match_result' must contain only 0 (= first player lost) or 1 (= first player won) values.")
  }

  # Map each match's players to integer indices (players sorted alphabetically,
  # matching the row order of initialize_ratings(sort(players), ...))
  players <- sort(unique(c(df$A, df$B)))
  p1_inds <- match(df$A, players)
  p2_inds <- match(df$B, players)

  # Work on plain numeric vectors inside the loop (no data.frame access per game)
  r <- current_ratings$Ratings
  X <- df$match_result
  n <- length(X)
  outcome_prob <- numeric(n)

  # Sequential game-by-game Elo updates
  for (g in seq_len(n)) {
    i <- p1_inds[g]
    j <- p2_inds[g]

    # Predicted probability that player A wins (same formula as winning_prob_elo)
    p <- 1 / (1 + 10^(-(r[i] - r[j]) / 400))

    # Probability of the observed outcome
    outcome_prob[g] <- if (X[g] == 1) p else 1 - p

    # Elo update (same as ratings_update_elo)
    upd <- K_guess * (X[g] - p)
    r[i] <- r[i] + upd
    r[j] <- r[j] - upd
  }

  # Prevent log(0) by imposing a minimum probability
  outcome_prob <- pmax(outcome_prob, 1e-6)

  # Return the negative log-likelihood for the current K value
  return(-sum(log(outcome_prob)))
})

#' Negative Log-Likelihood for PELO Ratings
#'
#' Computes the negative log-likelihood of a set of PELO ratings given observed match outcomes.
#' This function is intended for use in optimization to estimate the best K-factors
#' for both ratings and pairwise advantage updates.
#'
#' @param KK2_guess Numeric vector of length 2: `1` = Elo K, `2` = P-Elo K2
#'
#' @param data A dataframe of match results with columns: `Time`, `A`, `B`, `match_result`.
#'             `match_result` must be binary: 1 if player A won, 0 if player B won.
#' @param current_ratings A dataframe with columns `Player` and `Ratings` containing
#'                        the current ratings of all players.
#' @param current_pair_adv_mat Numeric matrix of pairwise advantages between players.
#'                             Must be square with row and column names equal to player names.
#'
#' @return Numeric, the negative log-likelihood of the match outcomes given the current ratings,
#'         pairwise advantage matrix, and K-factors.
#'
#' @details
#' Loops over all matches, computes predicted probability of the observed outcome using
#' PELO ratings with pairwise advantages, updates ratings and pairwise advantage matrix
#' sequentially, and accumulates the log-likelihood. A minimum probability of 1e-6 is used
#' to prevent numerical issues with log(0).
#'
#' @examples
#' # Suppose df contains match data, ratings contains current ratings,
#' # and pair_adv_mat contains pairwise advantages
#' # get_negloglike_pelo(c(32,16), df, ratings, pair_adv_mat)
#'
#' @export
get_negloglike_pelo <- cmpfun(function(KK2_guess, data, current_ratings, current_pair_adv_mat){

  K_guess <- KK2_guess[1]   # Elo K-factor for ratings update
  K2_guess <- KK2_guess[2]  # P-Elo K2-factor for pairwise advantage update

  df <- data

  # Ensure the dataframe has correct column names
  if(!all(colnames(df) == c("Time", "A", "B", "match_result"))){
    stop("Use colnames c('Time','A','B','match_result') for your dataset.")
  }

  # Ensure match outcomes are binary win/lose
  if (!all(df$match_result %in% c(0, 1))) {
    stop("The column 'match_result' must contain only 0 (= first player lost) or 1 (= first player won) values.")
  }

  # Map each match's players to integer indices (players sorted alphabetically,
  # matching the row order of initialize_ratings(sort(players), ...))
  players <- sort(unique(c(df$A, df$B)))
  p1_inds <- match(df$A, players)
  p2_inds <- match(df$B, players)

  # Pairwise advantage matrix: precompute linear indices of A[p1, p2] and A[p2, p1]
  pair_adv_mat <- current_pair_adv_mat
  idx <- .pair_adv_lin_index(pair_adv_mat, df$A, df$B)

  r <- current_ratings$Ratings
  X <- df$match_result
  n <- length(X)
  outcome_prob <- numeric(n)

  # Sequential game-by-game P-Elo updates
  for (g in seq_len(n)) {
    i <- p1_inds[g]
    j <- p2_inds[g]
    ab <- idx$ab[g]
    ba <- idx$ba[g]

    # Predicted probability that player A wins (same formula as winning_prob_pelo)
    p <- 1 / (1 + 10^(-(r[i] + pair_adv_mat[ab] - r[j]) / 400))

    # Probability of the observed outcome
    outcome_prob[g] <- if (X[g] == 1) p else 1 - p

    # Rating update (same as ratings_update_pelo)
    upd <- K_guess * (X[g] - p)
    r[i] <- r[i] + upd
    r[j] <- r[j] - upd

    # Pairwise advantage update (same as pairadv_update_pelo)
    upd2 <- K2_guess * (X[g] - p)
    pair_adv_mat[ab] <- pair_adv_mat[ab] + upd2
    pair_adv_mat[ba] <- pair_adv_mat[ba] - upd2
  }

  # Prevent log(0) by imposing a minimum probability
  outcome_prob <- pmax(outcome_prob, 1e-6)

  # Return negative log-likelihood for current KK2_guess values
  return(-sum(log(outcome_prob)))
})

#' Run Elo Rating Updates Across a Dataset
#'
#' Sequentially updates Elo ratings for all matches in a dataset and tracks predicted
#' probabilities and rating history. Supports adding new players dynamically.
#'
#' @param K Numeric, Elo K-factor for rating updates.
#' @param data A dataframe of match results with columns: `Time`, `A`, `B`, `match_result`.
#'             `match_result` must be binary: 1 if player A won, 0 if player B won.
#' @param current_ratings Dataframe with columns `Player` and `Ratings` for the current ratings.
#' @param player_id_mapping Dataframe with columns `Names` and `ID` mapping player names to unique IDs.
#' @param new_player_initial_rating Numeric, initial rating assigned to any new players (default 1500).
#' @param keep_history TRUE/FALSE, whether to keep the rating history. Memory issues may arise when the number of players and number of games are large.
#'
#' @return A list containing:
#' \describe{
#'   \item{ratings}{Dataframe of final ratings after all matches.}
#'   \item{ratings_hist}{List of ratings after each match.}
#'   \item{K}{Numeric, the Elo K-factor used.}
#'   \item{type}{Character, always "ELO".}
#'   \item{preds}{Numeric vector of predicted probabilities for each match outcome.}
#'   \item{player_id_mapping}{Updated dataframe mapping player names to IDs including new players.}
#' }
#'
#' @details
#' The function loops over all matches, updates player ratings sequentially, and predicts
#' match probabilities. If any new players appear, they are added with `new_player_initial_rating`
#' and assigned new IDs.
#'
#' @examples
#' # Suppose df contains match data and ratings contains current ratings
#' # run_elo(32, df, ratings, player_id_mapping)
#'
#' @export
run_elo <- cmpfun(function(K, data, current_ratings, player_id_mapping, new_player_initial_rating = 1500, keep_history = FALSE) {

  K_guess <- K
  df <- data

  # Column check: ensure the dataframe has the required columns
  if (!all(colnames(df) == c("Time", "A", "B", "match_result"))) {
    stop("Use colnames c('Time','A','B','match_result') for your dataset.")
  }

  # Ensure match outcomes are binary: 0 = first player lost, 1 = first player won
  if (!all(df$match_result %in% c(0, 1))) {
    stop("The column 'match_result' must contain only 0 (= first player lost) or 1 (= first player won) values.")
  }

  preds <- numeric(nrow(df))  # vector to store predicted probabilities for each match

  ratings_hist <- vector("list", nrow(df) + 1)  # list to store ratings after each match
  if(keep_history){
    ratings_hist[[1]] <- current_ratings           # initial ratings before any match
  }

  # Initialize ratings table
  ratings <- current_ratings

  # Validate player_id_mapping structure
  if (!all(c("Names", "ID") %in% colnames(player_id_mapping))) {
    stop("player_id_mapping must have columns 'Names' and 'ID'")
  }

  # Identify all players appearing in the current dataset
  players_in_matches <- unique(c(df$A, df$B))

  # Identify new players not already in player_id_mapping
  new_players <- setdiff(players_in_matches, player_id_mapping$Names)

  if (length(new_players) > 0) {
    # Assign new IDs starting after the current maximum ID
    new_ids <- max(player_id_mapping$ID) + seq_along(new_players)
    player_id_mapping <- rbind(
      player_id_mapping,
      data.frame(Names = new_players, ID = new_ids)
    )

    # Add new players to ratings with the default initial rating
    ratings <- rbind(
      ratings,
      data.frame(Player = new_players, Ratings = rep(new_player_initial_rating, length(new_players)))
    )
  }

  # Integer indices of both players in the ratings table, for every match
  p1_inds <- match(df$A, ratings$Player)
  p2_inds <- match(df$B, ratings$Player)

  # Work on a plain numeric vector inside the loop (no data.frame access per game)
  r <- ratings$Ratings
  X <- df$match_result

  # Loop over each match to update ratings and record predicted probabilities
  for (g in seq_along(X)) {
    i <- p1_inds[g]
    j <- p2_inds[g]

    # Predict winning probability for player A (same formula as winning_prob_elo)
    p <- 1 / (1 + 10^(-(r[i] - r[j]) / 400))
    preds[g] <- p

    # Update ratings according to Elo formula (same as ratings_update_elo)
    upd <- K_guess * (X[g] - p)
    r[i] <- r[i] + upd
    r[j] <- r[j] - upd

    if(keep_history){
      # Save current ratings snapshot to history
      ratings$Ratings <- r
      ratings_hist[[g + 1]] <- ratings
    }
  }
  ratings$Ratings <- r

  # Return final ratings, full rating history, K-factor, predictions, and updated player ID mapping
  return(list(
    ratings = ratings,
    ratings_hist = ratings_hist,
    K = K_guess,
    type = "ELO",
    preds = preds,
    player_id_mapping = player_id_mapping
  ))
})

#' Run PELO Rating Updates Across a Dataset
#'
#' Sequentially updates PELO ratings for all matches in a dataset, including pairwise
#' advantages, and tracks predicted probabilities and full rating history. Supports
#' dynamically adding new players that appear in the dataset.
#'
#' @param KK2 Numeric vector of length 2: `1` = Elo K, `2` = P-Elo K2
#'
#' @param data A dataframe of match results with columns: `Time`, `A`, `B`, `match_result`.
#'             `match_result` must be binary: 1 if player A won, 0 if player B won.
#' @param current_ratings Dataframe with columns `Player` and `Ratings` containing the
#'                        current ratings of all players.
#' @param current_pair_adv_mat Numeric square matrix of pairwise advantages between players,
#'                             with row and column names corresponding to player names.
#' @param player_id_mapping Dataframe with columns `Names` and `ID` mapping player names to unique IDs.
#' @param new_player_initial_rating Numeric, the initial rating assigned to any new players
#'                                  not in the current ratings (default 1500).
#' @param keep_history TRUE/FALSE, whether to keep the rating history. Memory issues may arise when the number of players and number of games are large.
#'
#' @return A list containing:
#' \describe{
#'   \item{ratings}{Dataframe of final ratings after all matches.}
#'   \item{ratings_hist}{List of ratings after each match.}
#'   \item{pair_adv}{Final pairwise advantage matrix.}
#'   \item{pair_adv_hist}{List of pairwise advantage matrices after each match.}
#'   \item{K}{Numeric, Elo K-factor used for ratings updates.}
#'   \item{K2}{Numeric, P-Elo K2-factor used for pairwise advantage updates.}
#'   \item{type}{Character, always "PELO".}
#'   \item{preds}{Numeric vector of predicted probabilities for each match outcome.}
#'   \item{player_id_mapping}{Updated dataframe mapping player names to IDs including any new players.}
#' }
#'
#' @details
#' The function loops over all matches in the dataset, updating player ratings and
#' pairwise advantages sequentially. If new players appear in the dataset, they are
#' added to the ratings and pairwise advantage matrix with the specified initial rating
#' and zero initial pairwise advantage. Predicted probabilities for each match are
#' computed based on the PELO formula.
#'
#' @examples
#' # Suppose df contains match data, ratings contains current ratings,
#' # pair_adv_mat contains pairwise advantages, and player_id_mapping exists
#' # run_pelo(c(32,16), df, ratings, pair_adv_mat, player_id_mapping)
#'
#' @export
run_pelo <- cmpfun(function(KK2, data, current_ratings, current_pair_adv_mat, player_id_mapping, new_player_initial_rating = 1500, keep_history = FALSE) {

  K_guess <- KK2[1]    # Elo K-factor for ratings update
  K2_guess <- KK2[2]   # P-Elo K2-factor for pairwise advantage update

  df <- data
  # Ensure dataframe has correct column names
  if (!all(colnames(df) == c("Time", "A", "B", "match_result"))) {
    stop("Use colnames c('Time','A','B','match_result') for your dataset.")
  }

  # Ensure match outcomes are binary: 0 = first player lost, 1 = first player won
  if (!all(df$match_result %in% c(0, 1))) {
    stop("The column 'match_result' must contain only 0 (= first player lost) or 1 (= first player won) values.")
  }

  preds <- numeric(nrow(df))  # vector to store predicted probabilities for each match

  # Lists to store ratings and pairwise advantage history after each match
  ratings_hist <- vector("list", nrow(df) + 1)
  if(keep_history){
    ratings_hist[[1]] <- current_ratings
  }

  pair_adv_hist <- vector("list", nrow(df) + 1)
  if(keep_history){
    pair_adv_hist[[1]] <- current_pair_adv_mat
  }

  # Initialize ratings and pairwise advantage matrix
  ratings <- current_ratings
  pair_adv_mat <- current_pair_adv_mat

  # Validate player_id_mapping structure
  if (!all(c("Names", "ID") %in% colnames(player_id_mapping))) {
    stop("player_id_mapping must have columns 'Names' and 'ID'")
  }

  # Identify all players in current dataset
  players_in_matches <- unique(c(df$A, df$B))

  # Identify new players not already in player_id_mapping
  new_players <- setdiff(players_in_matches, player_id_mapping$Names)

  if (length(new_players) > 0) {
    # Assign new IDs to new players
    new_ids <- max(player_id_mapping$ID) + seq_along(new_players)
    player_id_mapping <- rbind(
      player_id_mapping,
      data.frame(Names = new_players, ID = new_ids)
    )

    # Add new players to ratings table with default initial rating
    ratings <- rbind(
      ratings,
      data.frame(Player = new_players, Ratings = rep(new_player_initial_rating, length(new_players)))
    )

    # Extend pairwise advantage matrix to include new players
    old_players <- rownames(pair_adv_mat)
    all_players <- c(old_players, new_players)
    new_pair_adv_mat <- matrix(0, nrow = length(all_players), ncol = length(all_players),
                               dimnames = list(all_players, all_players))

    # Copy existing pairwise advantage values into the new matrix
    new_pair_adv_mat[old_players, old_players] <- pair_adv_mat
    pair_adv_mat <- new_pair_adv_mat
  }

  # Integer indices of both players in the ratings table, and linear indices of
  # A[p1, p2] and A[p2, p1] in the pairwise advantage matrix, for every match
  p1_inds <- match(df$A, ratings$Player)
  p2_inds <- match(df$B, ratings$Player)
  idx <- .pair_adv_lin_index(pair_adv_mat, df$A, df$B)

  # Work on a plain numeric vector inside the loop (no data.frame access per game)
  r <- ratings$Ratings
  X <- df$match_result

  # Loop over each match
  for (g in seq_along(X)) {
    i <- p1_inds[g]
    j <- p2_inds[g]
    ab <- idx$ab[g]
    ba <- idx$ba[g]

    # Predict probability of player A winning (same formula as winning_prob_pelo)
    p <- 1 / (1 + 10^(-(r[i] + pair_adv_mat[ab] - r[j]) / 400))
    preds[g] <- p

    # Update player ratings (same as ratings_update_pelo)
    upd <- K_guess * (X[g] - p)
    r[i] <- r[i] + upd
    r[j] <- r[j] - upd
    if(keep_history){
      ratings$Ratings <- r
      ratings_hist[[g + 1]] <- ratings  # save snapshot of ratings
    }

    # Update pairwise advantage matrix (same as pairadv_update_pelo)
    upd2 <- K2_guess * (X[g] - p)
    pair_adv_mat[ab] <- pair_adv_mat[ab] + upd2
    pair_adv_mat[ba] <- pair_adv_mat[ba] - upd2
    if(keep_history){
      pair_adv_hist[[g + 1]] <- pair_adv_mat  # save snapshot of pairwise advantages
    }
  }
  ratings$Ratings <- r

  # Return final ratings, full history, pairwise advantages, K-factors, predictions, and updated player IDs
  return(list(
    ratings = ratings,
    ratings_hist = ratings_hist,
    pair_adv = pair_adv_mat,
    pair_adv_hist = pair_adv_hist,
    K = K_guess,
    K2 = K2_guess,
    type = "PELO",
    preds = preds,
    player_id_mapping = player_id_mapping
  ))
})

#' Compute Historical Pairwise Win Probabilities
#'
#' Calculates the predicted win probabilities for every unique pair of players
#' across a series of rating snapshots. Optionally incorporates pairwise advantages
#' (PELO) if provided.
#'
#' @param ratings_history_obj A list of dataframes. Each dataframe should have at least
#'                            two columns: the first column contains player names and
#'                            the second column contains their ratings at a given time point.
#' @param pair_adv_mat_history_obj Optional. A list of matrices of pairwise advantages,
#'                                 corresponding to each time point in `ratings_history_obj`.
#'                                 If `NULL`, pairwise advantage is assumed zero.
#'
#' @return A numeric matrix of dimensions (time points × player pairs). Each column corresponds
#'         to a unique player pair in the format `"player_i_vs_player_j"`, and each row corresponds
#'         to a time point. Each element is the predicted probability that player_i defeats player_j
#'         at that time point.
#'
#' @details
#' - The function computes probabilities for all unique player pairs using the PELO formula:
#'   \code{winning_prob_pelo(r_i, r_j, adv_ij)}.
#' - If `pair_adv_mat_history_obj` is `NULL`, pairwise advantage is set to zero for all pairs.
#' - To avoid combinatorial explosion, the function stops if the number of player pairs exceeds 100.
#'
#'
#' @export
historical_win_prob <- function(ratings_history_obj, pair_adv_mat_history_obj = NULL){

  l <- length(ratings_history_obj)           # number of time points (matches or snapshots)
  players <- ratings_history_obj[[1]][,1]    # assuming first column contains player names
  n_players <- length(players)               # number of players

  # Check for combinatorial explosion: too many pairwise probabilities
  if(choose(n_players, 2) > 100){
    stop("Too many winning probabilities to recover. Try smaller number of players.")
  }

  # Prepare a matrix to hold win probabilities: rows = time points, cols = player pairs
  n_pairs <- choose(n_players, 2)
  win_prob_mat <- matrix(NA, nrow = l, ncol = n_pairs)

  # Column names in format "player_i_vs_player_j"
  pair_names <- character(n_pairs)

  pair_idx <- 1  # column counter

  # Loop over all unique player pairs
  for(i in 1:(n_players-1)){
    for(j in (i+1):n_players){
      player_i <- players[i]
      player_j <- players[j]

      # Vector to store win probability time series for this pair
      p_ij_ts <- numeric(l)

      # Loop over time points
      for(t in 1:l){
        ratings <- ratings_history_obj[[t]]       # ratings at time t
        r_i <- ratings[ratings[,1] == player_i, 2]  # rating of player i
        r_j <- ratings[ratings[,1] == player_j, 2]  # rating of player j

        # Extract pairwise advantage if provided, else zero
        if(!is.null(pair_adv_mat_history_obj)){
          adv_mat <- pair_adv_mat_history_obj[[t]]
          adv_ij <- adv_mat[player_i, player_j]
        } else {
          adv_ij <- 0
        }

        # Compute winning probability using Elo formula with optional pairwise advantage
        p_ij_ts[t] <- winning_prob_pelo(r_i, r_j, adv_ij)
      }

      # Store the time series in the matrix
      win_prob_mat[, pair_idx] <- p_ij_ts
      # Assign column name
      pair_names[pair_idx] <- paste(player_i, "vs", player_j, sep = "_")
      pair_idx <- pair_idx + 1
    }
  }

  # Assign column and row names to the output matrix
  colnames(win_prob_mat) <- pair_names
  rownames(win_prob_mat) <- paste0("time_", 1:l)

  return(win_prob_mat)  # matrix of pairwise win probabilities over time
}

#' Simulate Hypothetical Value Betting Profits
#'
#' Simulates bettor profits using bootstrap sampling, where the bettor only bets
#' when bookmaker odds offer positive expected value according to their probability estimates.
#'
#' @param bookmaker_preds Numeric vector of bookmaker probabilities for team A winning.
#' @param bettor_preds Numeric vector of bettor probabilities for team A winning.
#' @param actual_outcomes Numeric vector of actual match outcomes: 1 if team A won, 0 if team B won.
#' @param n_sims Integer. Number of bootstrap simulations.
#' @param n_bets Integer. Number of bets sampled per simulation (with replacement).
#'
#' @return Numeric vector of length `n_sims`, each element being total profit from one simulation.
#'
#' @export
hypo_bet_scheme_value <- cmpfun(function(bookmaker_preds, bettor_preds, actual_outcomes, n_sims, n_bets) {

  # Input checks
  if(length(bookmaker_preds) != length(bettor_preds)){
    stop("Number of probabilities from bookmaker and bettor do not match!")
  }
  if(length(bookmaker_preds) != length(actual_outcomes)){
    stop("Number of probabilities from bookmaker/bettor do not match the number of actual outcomes!")
  }

  # Convert bookmaker probabilities into odds
  bookmaker_odds_A <- 1 / bookmaker_preds
  bookmaker_odds_B <- 1 / (1 - bookmaker_preds)

  # Bettor probabilities
  bettor_prob_A <- bettor_preds
  bettor_prob_B <- 1 - bettor_preds

  # Compute expected value (EV) for each side
  EV_A <- bettor_prob_A * bookmaker_odds_A - 1
  EV_B <- bettor_prob_B * bookmaker_odds_B - 1

  # Determine side to bet on (only bet if EV > 0)
  bettor_side <- rep(NA_character_, length(bookmaker_preds))
  bettor_side[EV_A > EV_B & EV_A > 0] <- "A"
  bettor_side[EV_B > EV_A & EV_B > 0] <- "B"

  # Corresponding odds for the chosen side
  chosen_odds <- ifelse(bettor_side == "A", bookmaker_odds_A,
                        ifelse(bettor_side == "B", bookmaker_odds_B, NA))

  # Determine if chosen side wins (1 = profit, 0 = loss)
  chosen_side_wins <- ifelse(
    (bettor_side == "A" & actual_outcomes == 1) |
      (bettor_side == "B" & actual_outcomes == 0),
    1,
    0
  )

  # Matches that have a bet (non-NA side)
  valid_idx <- which(!is.na(bettor_side))
  if(length(valid_idx) == 0){
    return(numeric(n_sims))
  }

  # Profit of each possible bet: win = odds-1, loss = -1
  bet_profit <- chosen_side_wins[valid_idx] * (chosen_odds[valid_idx] - 1) +
    (1 - chosen_side_wins[valid_idx]) * (-1)

  # Bootstrap: each simulation samples n_bets bets with replacement.
  # Simulations are drawn in blocks; the random draws are the same, in the same
  # order, as drawing one simulation at a time.
  m <- min(n_bets, length(valid_idx))
  profits <- numeric(n_sims)
  block <- max(1L, floor(2e7 / m))
  for (s0 in seq(1, n_sims, by = block)) {
    k <- min(block, n_sims - s0 + 1)
    draws <- sample.int(length(valid_idx), size = k * m, replace = TRUE)
    profits[s0:(s0 + k - 1)] <- colSums(matrix(bet_profit[draws], nrow = m))
  }

  return(profits)
})

#' Optimize Elo K-Factor via Maximum Likelihood
#'
#' This function estimates the optimal Elo K-factor by minimizing the
#' negative log-likelihood of match outcomes using one-dimensional
#' bounded optimization.
#'
#' The optimization is performed using \code{\link[stats]{optim}} with
#' the \code{"Brent"} method, which is appropriate for scalar parameters.
#'
#' @param data A data frame containing match results used for training
#'   the Elo model. This object is passed directly to
#'   \code{get_negloglike_elo}.
#' @param initial_ratings A named numeric vector of initial Elo ratings
#'   for all competitors.
#' @param k_init Numeric scalar specifying the initial value of the
#'   K-factor for optimization. Default is 30.
#' @param lower Numeric scalar specifying the lower bound for the
#'   K-factor. Default is -500.
#' @param upper Numeric scalar specifying the upper bound for the
#'   K-factor. Default is 500.
#'
#' @return A numeric scalar representing the optimized K-factor value.
#'
#' @details
#' This function assumes the existence of a user-defined function
#' \code{get_negloglike_elo} that computes the negative log-likelihood
#' of observed outcomes given a K-factor, match data, and current
#' ratings.
#'
#' @seealso \code{\link[stats]{optim}}
#'
#'
#' @export
optimize_elo_k <- function(
    data,
    initial_ratings,
    k_init = 30,
    lower = -500,
    upper = 500
) {
  elo_optimized <- optim(
    par = k_init,
    fn = get_negloglike_elo,
    method = "Brent",
    lower = lower,
    upper = upper,
    data = data,
    current_ratings = initial_ratings
  )

  return(elo_optimized$par)
}

#' Optimize PElo (K,K2)-Factors via Maximum Likelihood
#'
#' This function estimates the optimal pair of PElo (K,K2)-factors by minimizing
#' the negative log-likelihood of match outcomes using bounded
#' multi-parameter optimization.
#'
#' Optimization is performed using \code{\link[stats]{optim}} with the
#' \code{"L-BFGS-B"} method, which supports box constraints for multiple
#' parameters.
#'
#' @param data A data frame containing match results used for training
#'   the PElo model. This object is passed directly to
#'   \code{get_negloglike_pelo}.
#' @param initial_ratings A named numeric vector of initial PElo ratings
#'   for all competitors.
#' @param initial_pair_adv_mat A numeric matrix representing the initial
#'   pairwise advantage parameters.
#' @param kk2_init Numeric vector of length two specifying the initial
#'   values of the PElo K-factors. Default is \code{c(30, 30)}.
#' @param lower Numeric vector of length two specifying the lower bounds
#'   for the K-factors. Default is \code{c(-500, -500)}.
#' @param upper Numeric vector of length two specifying the upper bounds
#'   for the K-factors. Default is \code{c(500, 500)}.
#'
#' @return A numeric vector of length two containing the optimized
#'   (K,K2)-factor values.
#'
#' @details
#' This function assumes the existence of a user-defined function
#' \code{get_negloglike_pelo} that computes the negative log-likelihood
#' of observed outcomes given a vector of (K,K2)-factors, match data,
#' current ratings, and a pairwise advantage matrix.
#'
#' @seealso \code{\link[stats]{optim}}
#'
#'
#' @export
optimize_pelo_kk2 <- function(
    data,
    initial_ratings,
    initial_pair_adv_mat,
    kk2_init = c(30, 30),
    lower = c(-500, -500),
    upper = c(500, 500)
) {
  pelo_optimized <- optim(
    par = kk2_init,
    fn = get_negloglike_pelo,
    method = "L-BFGS-B",
    lower = lower,
    upper = upper,
    data = data,
    current_ratings = initial_ratings,
    current_pair_adv_mat = initial_pair_adv_mat
  )

  return(pelo_optimized$par)
}
