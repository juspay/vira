# git-effectful

A Haskell library for Git operations using the [effectful](https://hackage.haskell.org/package/effectful) effect system.

## Example

```haskell
import Effectful.Git

-- Fetch remote branches
branches <- remoteBranches "https://github.com/user/repo.git"

-- Access commit information
let commits = Map.elems branches
let firstCommit = head commits
putStrLn $ commitMessage firstCommit
```
