#!/usr/bin/env bash
# Download the tennis data, pinned to the exact commit used for the results in
# this repository: the deuce R package's data folder (github.com/skoval/deuce),
# which holds a copy of Jeff Sackmann's tennis_atp results (atp_matches) and
# tennis-data.co.uk results with bookmaker odds (atp_odds).
#
# Usage:  bash data-raw/fetch_sources.sh        (run from the repository root)
set -euo pipefail

SRC="data-raw/sources"
DIR="$SRC/deuce"
SHA="6f2be7ac349dd7c7a6b42ebccf3419a558e24ab6"
mkdir -p "$SRC"
if [ -f "$DIR/data/atp_matches.RData" ] && [ -f "$DIR/data/atp_odds.RData" ] &&
   [ "$(git -C "$DIR" rev-parse HEAD 2>/dev/null)" = "$SHA" ]; then
  echo "ok    deuce @ ${SHA:0:7}"; exit 0
fi
rm -rf "$DIR"
git init -q "$DIR"
git -C "$DIR" remote add origin https://github.com/skoval/deuce
git -C "$DIR" sparse-checkout set --no-cone /data/atp_matches.RData /data/atp_odds.RData
git -C "$DIR" fetch -q --depth 1 --filter=blob:none origin "$SHA"
git -C "$DIR" checkout -q FETCH_HEAD
echo "got   deuce @ ${SHA:0:7}"
