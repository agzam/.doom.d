#!/usr/bin/env bash
# Real-command-loop e2e for window-undo / window-redo.
#
# A live `emacs -nw -Q' runs detached under screen(1) on a real pty;
# keys travel through the genuine event loop (unread-command-events),
# assertions run via emacsclient against the same process.  This covers
# what batch ERT cannot: redisplay timing, pre/post-command hook wiring,
# transient repeat dispatch, rapid queued input.
#
# Assertion style: while the transient popup is open it borrows screen
# space from sibling windows, so exact-edge fingerprints only match
# after it closes; inside the transient we compare buffer arrangement
# and ring lengths, then confirm the exact fingerprint after C-g.
set -u

DIR="$(cd "$(dirname "$0")" && pwd)"
SOCKDIR=/tmp/windows-e2e-sock
SOCK="$SOCKDIR/windows-e2e"
EMACS="${EMACS:-emacs}"
SESSION=windows-e2e

cleanup() {
  emacsclient -s "$SOCK" --eval '(kill-emacs)' >/dev/null 2>&1
  screen -S "$SESSION" -X quit >/dev/null 2>&1
}
trap cleanup EXIT

screen -S "$SESSION" -X quit >/dev/null 2>&1
rm -rf "$SOCKDIR"

screen -dmS "$SESSION" "$EMACS" -nw -Q -l "$DIR/e2e-driver.el"

EC() { emacsclient -s "$SOCK" --eval "$1" 2>/dev/null; }

up=""
for _ in $(seq 1 60); do
  [ "$(EC t)" = "t" ] && { up=1; break; }
  sleep 0.25
done
if [ -z "$up" ]; then
  echo "FATAL: emacs server never came up"
  exit 2
fi

PASS=0
FAIL=0
FEED() { EC "(e2e-feed \"$1\")" >/dev/null; sleep 0.35; }
CHECK() {
  local name="$1" expr="$2" got
  got="$(EC "$expr")"
  if [ "$got" = "t" ]; then
    PASS=$((PASS + 1)); echo "PASS  $name"
  else
    FAIL=$((FAIL + 1)); echo "FAIL  $name"
    echo "      expr: $expr => $got"
    echo "      state: $(EC '(e2e-state)')"
  fi
}

echo "== S1: transient round trip, one distinct layout per press =="
EC '(e2e-mark "L0")' >/dev/null
FEED "C-x 2"; EC '(e2e-mark "L1")' >/dev/null
FEED "C-x 3"; EC '(e2e-mark "L2")' >/dev/null
FEED "C-x b e2e-b RET"; EC '(e2e-mark "L3")' >/dev/null
CHECK "three steps recorded" '(= (e2e-back-len) 3)'
FEED "C-c w"
CHECK "transient open" '(e2e-transient-active-p)'
FEED "u"; CHECK "undo 1 lands at L2" '(and (e2e-at-mark-buffers-p "L2") (= (e2e-back-len) 2) (= (e2e-fwd-len) 1))'
CHECK "transient survives undo" '(e2e-transient-active-p)'
FEED "u"; CHECK "undo 2 lands at L1" '(and (e2e-at-mark-buffers-p "L1") (= (e2e-back-len) 1) (= (e2e-fwd-len) 2))'
FEED "u"; CHECK "undo 3 lands at L0" '(and (e2e-at-mark-buffers-p "L0") (= (e2e-back-len) 0) (= (e2e-fwd-len) 3))'
FEED "u"; CHECK "boundary undo is a no-op" '(and (e2e-at-mark-buffers-p "L0") (= (e2e-back-len) 0) (= (e2e-fwd-len) 3))'
FEED "r"; CHECK "redo 1 lands at L1" '(e2e-at-mark-buffers-p "L1")'
FEED "r"; CHECK "redo 2 lands at L2" '(e2e-at-mark-buffers-p "L2")'
FEED "r"; CHECK "redo 3 lands at L3" '(and (e2e-at-mark-buffers-p "L3") (= (e2e-fwd-len) 0))'
FEED "r"; CHECK "boundary redo is a no-op" '(and (e2e-at-mark-buffers-p "L3") (= (e2e-fwd-len) 0))'
FEED "C-g"
CHECK "transient closed" '(not (e2e-transient-active-p))'
CHECK "still at L3 after closing" '(e2e-at-mark-buffers-p "L3")'
# The first popup cycle shifts stacked-window dividers by one row
# (Emacs side-window space rounding, independent of undo: open+close
# alone does it).  Geometry settles after that; later exact checks
# compare against the settled layout.
EC '(e2e-mark "L3")' >/dev/null

echo "== S2: rapid queued keys behave exactly like slow ones =="
FEED "C-c w"
FEED "uuuuuuuuuu"
CHECK "10 rapid undos land exactly at L0" '(and (e2e-at-mark-buffers-p "L0") (= (e2e-back-len) 0) (= (e2e-fwd-len) 3))'
FEED "rrrrrrrrrr"
CHECK "10 rapid redos land exactly at L3" '(and (e2e-at-mark-buffers-p "L3") (= (e2e-fwd-len) 0))'
FEED "C-g"
CHECK "exact layout L3 after rapid round trip" '(e2e-at-mark-p "L3")'

echo "== S3: resize spree collapses into one step =="
FEED "C-c w"
BACK_BEFORE="$(EC '(e2e-back-len)')"
FEED "kkkk"
CHECK "spree recorded as one step" "(= (e2e-back-len) (1+ $BACK_BEFORE))"
FEED "u"
FEED "C-g"
CHECK "one undo unwinds the whole spree" '(e2e-at-mark-p "L3")'

echo "== S4: cursor position is never restored from history =="
FEED "C-x 1"
EC '(e2e-mark "T1-last")' >/dev/null
FEED "M->"
FEED "C-x 2"
FEED "M-<"
FEED "C-c w"
FEED "u"
CHECK "cursor stays where the user left it" '(= (window-point (selected-window)) 1)'
FEED "C-g"
CHECK "exact layout restored" '(e2e-at-mark-p "T1-last")'

echo "== S6: popup-free undo/redo is geometrically exact =="
FEED "C-x 1"
EC '(e2e-mark "S6-single")' >/dev/null
FEED "C-x 2"
EC '(e2e-mark "S6-two")' >/dev/null
FEED "C-c u"
CHECK "undo restores the exact geometry" '(e2e-at-mark-p "S6-single")'
FEED "C-c r"
CHECK "redo restores the exact geometry" '(e2e-at-mark-p "S6-two")'
FEED "C-c u"
CHECK "settled: second undo exact again" '(e2e-at-mark-p "S6-single")'

echo "== S5: per-tab isolation =="
T1_BACK="$(EC '(e2e-back-len)')"
FEED "C-x t 2"
CHECK "fresh tab starts with empty history" '(and (= (e2e-back-len) 0) (= (e2e-fwd-len) 0))'
EC '(e2e-mark "T2-0")' >/dev/null
FEED "C-x 2"
CHECK "tab2 records its own step" '(= (e2e-back-len) 1)'
FEED "C-c w"
FEED "u"
FEED "C-g"
CHECK "undo in tab2 stays in tab2" '(and (e2e-at-mark-p "T2-0") (= (e2e-back-len) 0) (= (e2e-fwd-len) 1))'
FEED "C-x t o"
CHECK "tab1 layout untouched" '(e2e-at-mark-p "T1-last")'
CHECK "tab1 ring untouched" "(= (e2e-back-len) $T1_BACK)"
EC '(tab-bar-close-tab 2)' >/dev/null

echo
echo "e2e: $PASS passed, $FAIL failed"
[ "$FAIL" -eq 0 ]
