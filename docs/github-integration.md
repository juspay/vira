# GitHub Integration

Vira integrates with GitHub via [GitHub Apps](https://docs.github.com/en/apps/creating-github-apps/about-creating-github-apps/about-creating-github-apps) to automatically build pull requests and report status via [Check Runs](https://docs.github.com/en/rest/checks/runs).

## Overview

When configured, Vira will:

- Receive webhook events when PRs are opened, reopened, or updated
- Create a GitHub Check Run with "Queued" status
- Trigger a Vira build for the PR's head commit
- Update the Check Run status as the build progresses (In Progress → Completed)
- Report the final result (Success/Failure/Cancelled) to GitHub

## Setup

### 1. Create a GitHub App

Follow GitHub's official guide: [Creating a GitHub App](https://docs.github.com/en/apps/creating-github-apps/setting-up-a-github-app/creating-a-github-app)

**Required settings:**

- **GitHub App name**: Choose a name (e.g., "Vira CI")
- **Homepage URL**: Your Vira instance URL (e.g., `https://vira.example.com`)
- **Webhook URL**: Your Vira webhook endpoint: `https://<your-vira-host>/webhook/github`
  - For local development/testing, use [smee.io](https://smee.io/) to forward webhooks to localhost
  - Example: `https://smee.io/your-unique-channel` (then run the smee client locally: `nix run nixpkgs#gosmee -- client https://smee.io/your-unique-channel https://localhost:5005/webhook/github`)
- **Webhook secret**: Generate a secure random string and save it to a file (you'll use this with `--github-webhook-secret-file`)

**Required permissions:**

| Permission        | Access       | Reason                       |
| ----------------- | ------------ | ---------------------------- |
| **Checks**        | Read & Write | Create and update check runs |
| **Pull requests** | Read         | Receive PR events            |

**Subscribe to events:**

- ✅ Pull request
- ✅ Push (optional, currently not used but planned)

After creating the app, note the **App ID** (shown on the app's settings page).

### 2. Generate a Private Key

On your GitHub App's settings page:

1. Scroll to "Private keys"
2. Click "Generate a private key"
3. Save the downloaded `.pem` file securely (e.g., `vira-github-app.pem`)

See: [Generating a private key](https://docs.github.com/en/apps/creating-github-apps/authenticating-with-a-github-app/managing-private-keys-for-github-apps#generating-private-keys)

### 3. Start Vira with GitHub Integration

Run Vira with the following required flags:

```sh
vira web \
  --github-app-id <APP_ID> \
  --github-app-private-key <PATH_TO_PEM_FILE> \
  --github-webhook-secret-file <PATH_TO_SECRET_FILE>
```

All three GitHub flags must be provided together for the integration to work. The webhook secret should be stored in a file and the path provided via `--github-webhook-secret-file`.

### 4. Install the GitHub App

Install your GitHub App on the repositories you want Vira to build:

1. Go to your GitHub App's settings page
2. Click "Install App" in the left sidebar
3. Select the account/organization
4. Choose repositories (all or specific repos)
5. Click "Install"

See: [Installing GitHub Apps](https://docs.github.com/en/apps/using-github-apps/installing-a-github-app-from-a-third-party)

### Automatic Repository Management

When you install the GitHub App, Vira automatically:

- Adds all selected repositories to the registry
- Schedules immediate refresh to fetch branches
- Skips repositories that already exist (idempotent)

**Repository Naming:** Repositories are named using GitHub's `owner/repo` format (e.g., `juspay/vira`) to ensure uniqueness.

**Uninstallation:** Removing the app or repositories from the installation will delete them from Vira (unless they have running jobs, in which case deletion is skipped with an error log).

### 5. Test with a Pull Request

Create a pull request in an installed repository:

- The PR must be from a **branch in the same repository** (fork PRs are not currently supported)
- Vira will automatically create a Check Run and start building
- Check the "Checks" tab on your PR to see the build status

## Webhook Endpoint

The webhook endpoint is available at:

```
https://<your-vira-host>/webhook/github
```

## Current Limitations

- **Fork PRs not supported**: Only PRs from branches in the same repository are currently supported
- **Push events**: Push events are received but not yet processed (planned feature)
- **Check Run details**: Build logs/output are not yet linked in the Check Run (access via Vira web UI)

## Troubleshooting

### Webhooks not received

1. Check webhook deliveries in GitHub App settings → Advanced → Recent Deliveries
2. Verify the webhook URL is correct and publicly accessible
3. For local development, ensure your smee client is running

### Check Runs not created

1. Ensure all three GitHub flags are provided when starting Vira
2. Check Vira logs for errors (look for `🪝` emoji in logs indicating webhook handler)
3. Verify the GitHub App has "Checks: Read & Write" permission

### Authentication errors

1. Verify the private key file path is correct and readable
2. Ensure the App ID matches your GitHub App
3. Check that the GitHub App is installed on the target repository

## References

- [GitHub Apps Documentation](https://docs.github.com/en/apps)
- [GitHub Webhooks Best Practices](https://docs.github.com/en/webhooks/using-webhooks/best-practices-for-using-webhooks)
- [GitHub Checks API](https://docs.github.com/en/rest/checks)
- [Webhook Events](https://docs.github.com/en/webhooks/webhook-events-and-payloads)
