# Build the tennis match table from the raw sources.
#
# Run from the repository root after data-raw/fetch_sources.sh:
#   Rscript data-raw/build_games.R
#
# Output: data/tennis_atp_tour_games.csv, one row per match, in playing order:
#   date          tournament start date
#   A, B          Sackmann player ids (A = the smaller id, so "A" has no special meaning)
#   y             1 if A won, 0 if B won
#   h             always 0 (no home side)
#   oddsA, oddsB  Pinnacle closing decimal odds (Grand Slam matches only, NA otherwise)
#   mktA          market probability that A wins, from the odds with the margin removed
#   A_name, B_name, surface, level, tournament, round

suppressMessages(library(utils))
src <- "data-raw/sources"
dir.create("data", showWarnings = FALSE)


# Keep two-way odds only when they look like real prices: both > 1 and a
# bookmaker margin between 0% and 15%. Anything else is set to NA.
clean_two_way <- function(oA, oB) {
  over <- 1 / oA + 1 / oB
  bad  <- is.na(over) | oA <= 1 | oB <= 1 | over < 1 | over > 1.15
  oA[bad] <- NA; oB[bad] <- NA
  list(oA = oA, oB = oB, mktA = (1 / oA) / (1 / oA + 1 / oB))
}

write_games <- function(d, file) {
  d <- d[order(d$date, d$ord), ]
  d$ord <- NULL
  rownames(d) <- NULL
  write.csv(d, file, row.names = FALSE)
  cat(sprintf("%-26s %6d games, %s to %s, %d with odds, %d draws/ties\n", file, nrow(d),
              min(d$date), max(d$date), sum(!is.na(d$oddsA)), sum(is.na(d$y))))
}

# ---------------------------------------------------------------------------
# ATP tour-level main-draw singles 1991-2024 (Grand Slams, Masters, ATP
#     250/500, Tour Finals), from the deuce package's copy of Jeff Sackmann's
#     tennis_atp data. Qualifying, Davis Cup, Challengers and Futures are
#     excluded, as are retirements, walkovers and defaults. Pinnacle odds are
#     attached to Grand Slam matches where the tennis-data.co.uk file has them.
# ---------------------------------------------------------------------------
e <- new.env(); suppressWarnings(load(file.path(src, "deuce/data/atp_matches.RData"), envir = e))
m <- get(ls(e)[1], envir = e)
m <- m[m$tourney_level %in% c("Grand Slams", "Masters", "250 or 500", "Tour Finals") & m$year >= 1991 &
       !grepl("^Q", m$round), ]
m <- m[!duplicated(m[, c("tourney_id", "winner_id", "loser_id", "round")]), ]
m <- m[!m$Retirement & !grepl("W/O|RET|DEF|ABD|Walk|unfinished", m$score, ignore.case = TRUE), ]
rr <- c(R128 = 1, R64 = 2, R32 = 3, R16 = 4, RR = 4.5, QF = 5, SF = 6, BR = 6.5, F = 7)
m$ord <- as.numeric(as.Date(m$tourney_start_date)) * 100 + rr[m$round] + m$match_num / 1e4
m <- m[order(m$ord), ]
wn <- trimws(m$winner_name); ln <- trimws(m$loser_name)
# Players are identified by Sackmann's player id (names can repeat); keep a readable label too.
wid <- as.character(m$winner_id); lid <- as.character(m$loser_id)
A <- pmin(wid, lid); B <- pmax(wid, lid); yA <- as.numeric(A == wid)
lab <- tapply(c(wn, ln), c(wid, lid), function(v) tail(v, 1))
# Pinnacle odds for Grand Slam matches: match on year, Slam, and both surnames
key_td <- function(nm) tolower(gsub("[^A-Za-z]", "", sub("\\s+\\S+\\.$", "", sub("\\s+[A-Z]\\.([A-Z]\\.)*$", "", nm))))
slam_of <- function(t) ifelse(grepl("Australian", t), "AO", ifelse(grepl("Roland|French", t), "RG",
                       ifelse(grepl("Wimbledon", t), "W", ifelse(grepl("Open", t) & grepl("US|U\\.S", t), "UO", NA))))
x2 <- get(ls(e2 <- {ee <- new.env(); load(file.path(src, "deuce/data/atp_odds.RData"), envir = ee); ee})[1], envir = e2)
x2 <- x2[x2$Comment == "Completed" & !is.na(x2$PSW) & !is.na(x2$PSL), ]
x2$k <- paste(format(x2$Date, "%Y"), slam_of(x2$Tournament), key_td(x2$Winner), key_td(x2$Loser))
x2 <- x2[!duplicated(x2$k) & !duplicated(x2$k, fromLast = TRUE), ]
gs <- m$tourney_level == "Grand Slams"
sur <- function(full) tolower(gsub("[^A-Za-z]", "", full))
oddsW <- oddsL <- rep(NA_real_, nrow(m))
cand <- split(seq_len(nrow(x2)), paste(format(x2$Date, "%Y"), slam_of(x2$Tournament)))
for (k in which(gs)) {
  ck <- cand[[paste(m$year[k], slam_of(m$tourney_name[k]))]]; if (is.null(ck)) next
  wfull <- sur(wn[k]); lfull <- sur(ln[k])
  hit <- ck[endsWith(wfull, key_td(x2$Winner[ck])) & endsWith(lfull, key_td(x2$Loser[ck])) &
            nchar(key_td(x2$Winner[ck])) >= 3 & nchar(key_td(x2$Loser[ck])) >= 3]
  if (length(hit) == 1) { oddsW[k] <- x2$PSW[hit]; oddsL[k] <- x2$PSL[hit] }
}
od <- clean_two_way(ifelse(yA == 1, oddsW, oddsL), ifelse(yA == 1, oddsL, oddsW))
tour <- data.frame(date = as.Date(m$tourney_start_date), period = NA_integer_, A = A, B = B, y = yA, h = 0,
                   oddsA = od$oA, oddsB = od$oB, mktA = od$mktA,
                   A_name = unname(lab[A]), B_name = unname(lab[B]), surface = m$surface,
                   level = m$tourney_level, tournament = m$tourney_name, round = m$round,
                   ord = seq_len(nrow(m)), stringsAsFactors = FALSE)
write_games(tour, "data/tennis_atp_tour_games.csv")
