# Wait, and read what the shell writes meanwhile.
#
# A plain `after` reads nothing, so the pty fills while it waits. The line
# editor redraws the whole line on each key, so a few lines fill it, and the
# shell then blocks on a write. A key sent next arrives while the shell is still
# in the editor's raw mode: Ctrl-C is then a byte, not SIGINT. -notransfer keeps
# what was read, so the patterns after the pause still see it.
match_max 1000000
proc pause {ms} {
  set end [expr {[clock milliseconds] + $ms}]
  while {[clock milliseconds] < $end} {
    expect -notransfer -timeout 0 -re {\A\b\B}
    after 10
  }
}
