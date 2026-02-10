# github-app-auth

GitHub App authentication library. Authenticate as a GitHub App installation to make API requests on behalf of the app.

Like [github-app](https://github.com/serokell/github-app) but simpler: no token caching, just stateless JWT creation and token exchange.

Uses the [github](https://hackage.haskell.org/package/github) package for API requests and [jose](https://hackage.haskell.org/package/jose) for JWT creation.

## Usage

```haskell
import GitHub.Auth.App (withAppAuth, AppAuthError)
import GitHub.Data.Id (Id (..))
import GitHub (executeRequest)

example :: IO (Either AppAuthError ())
example =
  withAppAuth
    (Id 12345)                -- App ID
    (Id 67890)                -- Installation ID
    "path/to/private-key.pem"
    $ \auth -> do
      -- Use auth for GitHub API requests
      executeRequest auth someGitHubRequest
```
