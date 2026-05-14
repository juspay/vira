#!/usr/bin/env bash
# Sample vira post-build hook: echoes the three env vars vira exports
# (VIRA_BRANCH, VIRA_COMMIT_ID, VIRA_REPO_CLONE_URL). Wired up by
# `just ci` so the hook integration is exercised on every local run.

set -euo pipefail

echo "[post-build-hook] VIRA_BRANCH=${VIRA_BRANCH}"
echo "[post-build-hook] VIRA_COMMIT_ID=${VIRA_COMMIT_ID}"
echo "[post-build-hook] VIRA_REPO_CLONE_URL=${VIRA_REPO_CLONE_URL:-<unset>}"
