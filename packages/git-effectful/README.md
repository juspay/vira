# git-effectful

A standalone Haskell library for Git operations using the [effectful](https://hackage.haskell.org/package/effectful) effect system.

## What it provides

- **Type-safe Git operations**: Strongly typed Git commit, branch, and repository representations
- **Effectful integration**: Clean integration with the effectful library for composable effects
- **Parsing utilities**: Robust parsing of Git command outputs using Megaparsec
- **Repository querying**: Efficient remote branch and commit information retrieval
- **Indexed data structures**: IxSet-based indexing for fast commit lookups

## Key types

- `Commit` - Git commit with ID, message, date, author, and email
- `CommitID` - Type-safe commit hash
- `BranchName` - Type-safe branch name
- `IxCommit` - Indexed set of commits for efficient querying

## Key functions

- `remoteBranches` - Fetch all remote branches with commit information
- `clone` - Create git clone process configurations
- `cloneAtCommit` - Clone at a specific commit

## Benefits

- **Effect system**: Leverages effectful for clean separation of concerns and testability
- **Type safety**: Prevents common Git-related bugs through strong typing
- **Performance**: Uses IxSet for efficient data indexing and querying
- **Composability**: Designed to integrate seamlessly with larger effectful applications
- **Standalone**: No dependencies on application-specific modules, making it reusable

## Usage

```haskell
import Effectful.Git

-- Fetch remote branches
branches <- remoteBranches "https://github.com/user/repo.git"

-- Access commit information
let commits = Map.elems branches
let firstCommit = head commits
putStrLn $ commitMessage firstCommit
```
