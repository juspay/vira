---
slug: cleanup
---

# Automatic Job Cleanup

Vira automatically removes old completed jobs to prevent unbounded disk usage growth.

## How It Works

- Runs **hourly** in the background
- Deletes **finished jobs only** (Success, Failure, or Killed)
- **Never touches active jobs** (Pending or Running)
- Removes both job data and workspace directories
- Jobs deleted **oldest first** (most recent history preserved)

## Configuration

### Retention Period

Configure via `--job-retention-days` (see [[cli#web-opts]]):

- **Default: 14 days** - Jobs older than two weeks are deleted
- **Custom:** Any positive number (e.g., `--job-retention-days 30` for 30 days)
- **Disable:** Set to `0` to turn off automatic cleanup

Jobs are considered "old" based on **completion time**, not creation time.

### CLI Usage

```bash
# Default - 14 day retention
vira web

# Custom retention - 30 days
vira web --job-retention-days 30

# Disable cleanup
vira web --job-retention-days 0
```

### Nix Module (Home Manager)

```nix
services.vira = {
  enable = true;
  jobRetentionDays = 30;  # Keep jobs for 30 days
};
```

## Monitoring

Cleanup activity appears in logs:

```
Found 15 jobs older than 14 days
🧹 Deleted job 123 (3d 2h 15m 30s old)
```

## Performance Tips

> [!tip]
> If restarts are slow after schema changes, reduce the retention period. Large workspace directories slow down the auto-reset process.

Monitor workspace disk usage:

```bash
du -sh state/workspace/*/jobs
```

To reclaim space quickly:

- Reduce `jobRetentionDays` temporarily
- Restart Vira to trigger cleanup
