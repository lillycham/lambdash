#!/usr/bin/env bash
# Build, then run every unit suite and every pty suite, and summarise.
#
# Run it inside `nix develop`, from anywhere in the repository:
#
#   test/run.sh            everything
#   test/run.sh --no-nix   skip test/nix.exp, which builds a flake
#
# The pty suites start the shell in the repository root and with a fresh
# XDG_STATE_HOME each, so personal approvals never leak in.
set -u
cd "$(dirname "$0")/.."

skip_nix=0
[ "${1:-}" = "--no-nix" ] && skip_nix=1

# The prompt is λ, so the suites need a UTF-8 locale.
case "${LC_ALL:-${LANG:-}}" in
  *UTF-8*|*utf8*) ;;
  *) if [ "$(uname)" = Darwin ]; then export LC_ALL=en_US.UTF-8; else export LANG=C.UTF-8; fi ;;
esac

echo "== build"
cabal build all >/dev/null || { cabal build all; exit 1; }

failed=()
echo "== unit"
unit=$(cabal test 2>&1)
status=$?
echo "$unit" | grep -E "^(FAIL|TODO|HANG) |^[0-9]+/[0-9]+ passed$"
[ $status -eq 0 ] || failed+=("unit")

echo "== pty"
# A suite that hangs is stopped, where timeout exists.
limit=""
command -v timeout >/dev/null && limit="timeout 600"
lambdash=$(cabal list-bin lambdash)
spike=$(cabal list-bin spike-jobs)
run_pty() {
  local name=$1 binary=$2 file=$3 log
  log=$($limit expect "$file" "$binary" 2>&1)
  if echo "$log" | grep -q "^ALL PASSED"; then
    printf '%-14s %3d passed\n' "$name" "$(echo "$log" | grep -c '^PASS')"
  else
    printf '%-14s %s\n' "$name" "$(echo "$log" | grep -m1 '^FAIL' || echo 'FAIL: no result')"
    failed+=("$name")
  fi
}
run_pty spike "$spike" spike/test.exp
for suite in externals pipelines environments jobs bg background blocks trust locale nix; do
  [ "$suite" = nix ] && [ $skip_nix -eq 1 ] && continue
  run_pty "$suite" "$lambdash" "test/$suite.exp"
done

if [ ${#failed[@]} -eq 0 ]; then
  echo "== all passed"
else
  echo "== failed: ${failed[*]}"
  exit 1
fi
