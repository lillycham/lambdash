#!/usr/bin/env bash
# cat for the job-control tests: copy lines from stdin to stdout.
#
# On macOS, a process stopped in a terminal read() and then continued gets
# EINTR, and GNU cat treats that as fatal (bash shows the same). Here an
# interrupted read is retried, so the tests check the shell's job control
# rather than the platform's restart semantics.
while true; do
  if IFS= read -r line; then
    printf '%s\n' "$line"
  elif [ $? -gt 128 ]; then
    continue
  else
    break
  fi
done
