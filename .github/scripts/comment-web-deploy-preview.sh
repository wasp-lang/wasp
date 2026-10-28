#!/usr/bin/env bash
# Creates or updates the PR comment with the status of the web preview deploy.
# We keep a single comment per PR and update it on every deploy.
#
# Required env vars (besides the default GitHub Actions ones):
#   GH_TOKEN, PR_NUMBER, COMMIT_SHA, JOB_STATUS, WRANGLER_OUTPUT_FILE_PATH

set -euo pipefail

COMMENT_MARKER="<!-- web-deploy-preview -->"
REPO_URL="$GITHUB_SERVER_URL/$GITHUB_REPOSITORY"

main() {
  local preview_url deployment_url
  preview_url=$(read_wrangler_preview_output '.preview_urls[0]')
  deployment_url=$(read_wrangler_preview_output '.deployment_urls[0]')

  local is_deployed=false
  if [[ "$JOB_STATUS" == "success" && -n "$preview_url" ]]; then
    is_deployed=true
  fi

  local body
  body=$(render_comment "$is_deployed" "$preview_url" "$deployment_url")
  upsert_comment "$body"

  if [[ "$is_deployed" != "true" ]]; then
    exit 1
  fi
}

read_wrangler_preview_output() {
  local jq_path="$1"
  if [[ -f "$WRANGLER_OUTPUT_FILE_PATH" ]]; then
    jq -r "select(.type == \"preview\") | $jq_path // empty" "$WRANGLER_OUTPUT_FILE_PATH"
  fi
}

render_comment() {
  local is_deployed="$1" preview_url="$2" deployment_url="$3"

  local status="❌ Deploy failed"
  if [[ "$is_deployed" == "true" ]]; then
    status="✅ Deployed"
  fi

  local rows
  local commit_url="$REPO_URL/commit/$COMMIT_SHA"
  rows=$(table_row "Latest commit" "<a href=\"$commit_url\"><code>${COMMIT_SHA:0:7}</code></a>")
  rows+=$(table_row "Status" "$status")
  if [[ -n "$preview_url" ]]; then
    rows+=$(table_row "Preview URL" "$(link "$preview_url")")
  fi
  if [[ -n "$deployment_url" ]]; then
    rows+=$(table_row "Deployment URL" "$(link "$deployment_url")")
  fi

  cat <<EOF
$COMMENT_MARKER
## Docs preview

<table>$rows</table>

[View logs]($REPO_URL/actions/runs/$GITHUB_RUN_ID)
EOF
}

table_row() {
  local label="$1" value="$2"
  echo "<tr><td><strong>$label</strong></td><td>$value</td></tr>"
}

link() {
  local url="$1"
  echo "<a href=\"$url\">$url</a>"
}

upsert_comment() {
  local body="$1"

  local existing_comment_id
  existing_comment_id=$(
    gh api "repos/$GITHUB_REPOSITORY/issues/$PR_NUMBER/comments" --paginate \
      --jq ".[] | select(.user.login == \"github-actions[bot]\" and (.body | startswith(\"$COMMENT_MARKER\"))) | .id" |
      head -n 1
  )

  if [[ -n "$existing_comment_id" ]]; then
    gh api --method PATCH "repos/$GITHUB_REPOSITORY/issues/comments/$existing_comment_id" -f body="$body" >/dev/null
  else
    gh api --method POST "repos/$GITHUB_REPOSITORY/issues/$PR_NUMBER/comments" -f body="$body" >/dev/null
  fi
}

main
