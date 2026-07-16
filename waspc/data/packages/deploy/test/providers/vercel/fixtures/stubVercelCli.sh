#!/bin/sh
# Stand-in for the real `vercel` CLI used only in tests: any invocation
# (--version, whoami, etc.) succeeds immediately so tests can exercise the
# command wiring without a real Vercel CLI/account.
exit 0
