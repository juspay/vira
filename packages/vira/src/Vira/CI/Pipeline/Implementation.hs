{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Vira.CI.Pipeline.Implementation (
  runPipeline,

  -- * Used in tests
  defaultPipeline,
  checkDomain,
  isLoopbackHost,
  isIpLiteral,
  sanitiseHeader,
  sanitiseHeaderName,
  sanitiseHeaderValue,
) where

import Prelude hiding (asks, id)

import Attic qualified
import Attic.Config (lookupEndpointWithToken)
import Attic.Types (AtticServer (..), AtticServerEndpoint)
import Attic.Url qualified
import Colog (Severity (..))
import Colog.Message (RichMessage)
import Control.Exception (try)
import Data.Aeson (eitherDecodeFileStrict)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List (lookup)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (splitOn, strip)
import Data.Text qualified as T
import DevourFlake (DevourFlakeArgs (..), devourFlake, prefetchFlakeInputs)
import DevourFlake.Result (DevourFlakeResult (..), SystemOutputs (..), extractSystems)
import Effectful
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext (..))
import Effectful.Concurrent.Async (Concurrent)
import Effectful.Dispatch.Dynamic
import Effectful.Environment (Environment, getEnvironment)
import Effectful.Error.Static (Error, throwError)
import Effectful.FileSystem (FileSystem, doesFileExist)
import Effectful.Git.Command.Clone qualified as Git
import Effectful.Git.Platform (detectPlatform)
import Effectful.Git.Types (Commit (id))
import Effectful.Process (Process)
import Effectful.Reader.Static qualified as ER
import Network.HTTP.Req (
  HttpException,
  NoReqBody (..),
  ReqBodyBs (..),
  defaultHttpConfig,
  header,
  ignoreResponse,
  req,
  responseTimeout,
  runReq,
  useURI,
 )
import Network.HTTP.Req qualified as Req
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
import System.FilePath ((</>))
import System.Nix.Core (nix)
import System.Nix.System (System (..))
import System.Process (proc)
import Text.URI (Authority (..), mkURI, unRText, uriAuthority, uriScheme)
import Vira.CI.Configuration qualified as Configuration
import Vira.CI.Context (ViraContext (..))
import Vira.CI.Error (ConfigurationError (..), PipelineError (..), pipelineToolError)
import Vira.CI.Pipeline.Effect
import Vira.CI.Pipeline.Process (runProcess)
import Vira.CI.Pipeline.Signoff qualified as Signoff
import Vira.CI.Pipeline.Type (BuildStage (..), CacheStage (..), Flake (..), HttpMethod (..), NixConfig (..), PostBuildStage (..), SignoffStage (..), ViraPipeline (..), WebhookConfig (..), allowedNixOptions, substituteVars, validateNixOptions)
import Vira.Environment.Tool.Tools.Attic qualified as AtticTool
import Vira.Environment.Tool.Type.ToolData (status)
import Vira.Environment.Tool.Type.Tools (attic)
import Vira.State.Type (Branch (..), Repo (..))

-- | Run the unified Pipeline effect
runPipeline ::
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , ER.Reader LogContext :> es
  , Error PipelineError :> es
  , Environment :> es
  ) =>
  PipelineEnv ->
  Eff (Pipeline : ER.Reader PipelineEnv : es) a ->
  Eff es a
runPipeline env program =
  ER.runReader env $
    interpret
      ( \_ -> \case
          Clone repo branch workspacePath -> cloneImpl repo branch workspacePath
          LoadConfig -> loadConfigImpl
          Build pipeline -> buildImpl pipeline
          Cache pipeline buildResults -> cacheImpl pipeline buildResults
          Signoff pipeline buildResults -> signoffImpl pipeline buildResults
          PostBuild pipeline buildResults -> postBuildImpl pipeline buildResults
      )
      program

-- | Implementation: Clone repository
cloneImpl ::
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , ER.Reader LogContext :> es
  , ER.Reader PipelineEnv :> es
  , Error PipelineError :> es
  , Environment :> es
  ) =>
  Repo ->
  Branch ->
  FilePath ->
  Eff es FilePath
cloneImpl repo branch workspacePath = do
  let projectDirName = "project"
  cloneProc <-
    Git.cloneAtCommit
      repo.cloneUrl
      branch.headCommit.id
      projectDirName

  logPipeline Info $ "Cloning repository at commit " <> toText branch.headCommit.id

  runProcess workspacePath cloneProc

  let clonedDir = workspacePath </> projectDirName
  logPipeline Info $ "Repository cloned to " <> toText clonedDir
  pure clonedDir

-- | Implementation: Load vira.hs configuration
loadConfigImpl ::
  ( FileSystem :> es
  , IOE :> es
  , Log (RichMessage IO) :> es
  , ER.Reader LogContext :> es
  , ER.Reader PipelineEnv :> es
  , Error PipelineError :> es
  ) =>
  Eff es ViraPipeline
loadConfigImpl = do
  env <- ER.ask @PipelineEnv
  let repoDir = env.viraContext.repoDir
      viraConfigPath = repoDir </> "vira.hs"
  doesFileExist viraConfigPath >>= \case
    True -> do
      logPipeline Info "Found vira.hs configuration file, applying customizations..."
      content <- liftIO $ decodeUtf8 <$> readFileBS viraConfigPath
      Configuration.applyConfig content env.viraContext defaultPipeline >>= \case
        Left err -> throwError $ PipelineConfigurationError $ InterpreterError err
        Right p -> do
          logPipeline Info "Successfully applied vira.hs configuration"
          pure $ patchPipelineForCli env.viraContext p
    False -> do
      logPipeline Info "No vira.hs found - using default pipeline"
      pure $ patchPipelineForCli env.viraContext defaultPipeline
  where
    -- When onlyBuild is enabled, restrict to current system and disable cache/signoff
    patchPipelineForCli :: ViraContext -> ViraPipeline -> ViraPipeline
    patchPipelineForCli ctx pipeline
      | ctx.onlyBuild =
          pipeline
            { -- Don't signoff when only building
              signoff = pipeline.signoff {enable = False}
            , -- Don't push to cache when only building
              cache = CacheStage {url = Nothing}
            , -- Only build for current system when only building
              build = BuildStage {flakes = pipeline.build.flakes, systems = []}
            , -- Don't fire webhooks when only building (webhooks are side effects)
              postBuild = PostBuildStage {webhooks = []}
            }
      | otherwise = pipeline

-- | Implementation: Build flakes
buildImpl ::
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , ER.Reader LogContext :> es
  , ER.Reader PipelineEnv :> es
  , Error PipelineError :> es
  ) =>
  ViraPipeline ->
  Eff es (NonEmpty BuildResult)
buildImpl pipeline = do
  logPipeline Info $ "Building " <> show (length pipeline.build.flakes) <> " flakes"
  -- Validate nix options against whitelist
  case validateNixOptions pipeline.nix.options of
    [] -> pass
    bad -> throwError $ PipelineConfigurationError $ MalformedConfig $ "Disallowed nix options: " <> show bad <> ". Allowed: " <> show allowedNixOptions
  -- Build each flake sequentially and return BuildResult for each
  forM pipeline.build.flakes $ \flake ->
    buildFlake pipeline.build.systems pipeline.nix flake

-- | Pretty-print DevourFlakeResult in a concise format
prettyDevourResult :: FilePath -> DevourFlakeResult -> Text
prettyDevourResult flakePath (DevourFlakeResult systems) =
  renderStrict $
    layoutPretty defaultLayoutOptions $
      vsep
        [ "Build outputs for" <+> pretty flakePath <> ":"
        , indent 2 $ vsep $ map prettySystem (Map.toList systems)
        ]
  where
    prettySystem :: (System, SystemOutputs) -> Doc ann
    prettySystem (System sys, SystemOutputs {byName}) =
      pretty sys
        <> ":"
        <+> pretty (Map.size byName)
        <+> "packages"
        <+> parens (hsep $ punctuate comma $ map pretty $ take 5 $ Map.keys byName)
        <> if Map.size byName > 5 then ", ..." else mempty

-- | Build a single flake
buildFlake ::
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , ER.Reader LogContext :> es
  , ER.Reader PipelineEnv :> es
  , Error PipelineError :> es
  ) =>
  [System] ->
  NixConfig ->
  Flake ->
  Eff es BuildResult
buildFlake systems nixCfg (Flake flakePath overrideInputs) = do
  env <- ER.ask @PipelineEnv
  let repoDir = env.viraContext.repoDir
  let args =
        DevourFlakeArgs
          { flakePath = flakePath
          , systems
          , outLink = Just (flakePath </> "result")
          , overrideInputs = overrideInputs
          , nixOptions = nixCfg.options
          }

  -- Prefetch flake inputs before building (for devourFlakePath and target flake)
  logPipeline Info "Prefetching flake inputs"
  runProcess repoDir $ proc nix $ prefetchFlakeInputs args

  -- Run build process from working directory
  logPipeline Info $ "Building flake at " <> toText flakePath
  runProcess repoDir $ proc nix $ devourFlake args

  -- Return relative path to result symlink (relative to repo root)
  let resultPath = flakePath </> "result"
  logPipeline Info $ "Build succeeded, result at " <> toText resultPath

  -- Parse the JSON result
  devourResult <- liftIO $ eitherDecodeFileStrict $ repoDir </> resultPath
  case devourResult of
    Left err ->
      throwError $ DevourFlakeMalformedOutput resultPath err
    Right parsed -> do
      logPipeline Info $ prettyDevourResult flakePath parsed
      pure $ BuildResult flakePath resultPath parsed

-- | Implementation: Push to cache
cacheImpl ::
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , ER.Reader LogContext :> es
  , ER.Reader PipelineEnv :> es
  , Error PipelineError :> es
  ) =>
  ViraPipeline ->
  NonEmpty BuildResult ->
  Eff es ()
cacheImpl pipeline buildResults = do
  env <- ER.ask @PipelineEnv
  let repoDir = env.viraContext.repoDir
  case pipeline.cache.url of
    Nothing -> do
      logPipeline Warning "Cache disabled, skipping"
    Just urlText -> do
      logPipeline Info $ "Pushing " <> show (length buildResults) <> " build results to cache"

      -- Parse cache URL
      (serverEndpoint, cacheName) <- case Attic.Url.parseCacheUrl urlText of
        Left err -> throwError $ parseErrorToPipelineError urlText err
        Right result -> pure result

      -- Get attic server info (token validated by lookupEndpointWithToken)
      server <- case do
        atticConfig <- env.tools.attic.status
        -- Get server name for endpoint (only if it has a token)
        serverName <-
          lookupEndpointWithToken atticConfig serverEndpoint
            & maybeToRight (AtticTool.MissingEndpoint serverEndpoint)
        -- Create server (token already validated by lookupEndpointWithToken)
        pure $ AtticServer serverName serverEndpoint of
        Left err -> throwError $ atticErrorToPipelineError urlText serverEndpoint err
        Right result -> pure result

      -- Push to cache - paths are relative to repoDir
      let pathsToPush = fmap (.resultPath) buildResults
      logPipeline Info $ "Pushing " <> show (length pathsToPush) <> " result files: " <> show (toList pathsToPush)
      let pushProc = Attic.atticPushProcess server cacheName pathsToPush
      runProcess repoDir pushProc
      logPipeline Info "Cache push succeeded"
  where
    parseErrorToPipelineError :: Text -> Attic.Url.ParseError -> PipelineError
    parseErrorToPipelineError url err =
      PipelineConfigurationError $
        MalformedConfig $
          "Invalid cache URL '" <> url <> "': " <> show err

    atticErrorToPipelineError :: Text -> AtticServerEndpoint -> AtticTool.ConfigError -> PipelineError
    atticErrorToPipelineError url _endpoint err =
      let suggestion = AtticTool.configErrorToSuggestion err
          msg = "Attic configuration error for cache URL '" <> url <> "': " <> show err
       in pipelineToolError msg (Just suggestion)

-- | Implementation: Create signoff (one per system)
signoffImpl ::
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , ER.Reader LogContext :> es
  , ER.Reader PipelineEnv :> es
  , Error PipelineError :> es
  ) =>
  ViraPipeline ->
  NonEmpty BuildResult ->
  Eff es ()
signoffImpl pipeline buildResults = do
  env <- ER.ask @PipelineEnv
  let commitId = env.viraContext.commitId
      mCloneUrl = env.viraContext.cloneUrl
      repoDir = env.viraContext.repoDir
  if pipeline.signoff.enable
    then do
      case mCloneUrl of
        Nothing ->
          throwError $
            pipelineToolError
              ("Signoff enabled but no remote URL is available. Add an 'origin' remote or disable signoff." :: Text)
              (Nothing :: Maybe Text)
        Just cloneUrl -> do
          -- Extract unique systems from all build results
          let systems = extractSystems $ fmap (.devourResult) (toList buildResults)
              signoffNames = fmap (\system -> "vira/" <> toString system) (toList systems)
          case nonEmpty signoffNames of
            Nothing -> throwError $ DevourFlakeMalformedOutput "build results" "No systems found in build results"
            Just names -> do
              -- Detect platform based on clone URL
              case detectPlatform cloneUrl of
                Nothing ->
                  throwError $
                    pipelineToolError
                      ("Signoff enabled but could not detect platform from clone URL: " <> cloneUrl <> ". Must be GitHub or Bitbucket.")
                      (Nothing :: Maybe Text)
                Just platform -> do
                  Signoff.performSignoff commitId platform repoDir names
    else
      logPipeline Warning "Signoff disabled, skipping"

-- | Implementation: Fire post-build webhooks
postBuildImpl ::
  ( Log (RichMessage IO) :> es
  , IOE :> es
  , ER.Reader LogContext :> es
  , ER.Reader PipelineEnv :> es
  , Error PipelineError :> es
  , Environment :> es
  ) =>
  ViraPipeline ->
  NonEmpty BuildResult ->
  Eff es ()
postBuildImpl pipeline _buildResults = do
  env <- ER.ask @PipelineEnv
  let ctx = env.viraContext
      hooks = pipeline.postBuild.webhooks
  if null hooks
    then logPipeline Info "No post-build webhooks configured, skipping"
    else do
      -- Build the base $VIRA_* substitution bindings from build context
      let viraBindings =
            [ ("VIRA_BRANCH", toText ctx.branch)
            , ("VIRA_COMMIT_ID", toText ctx.commitId)
            , ("VIRA_CLONE_URL", maybe "" identity ctx.cloneUrl)
            , ("VIRA_REPO_DIR", toText ctx.repoDir)
            , ("VIRA_ONLY_BUILD", if ctx.onlyBuild then "true" else "false")
            ]
      -- Read the operator-configured allowlist of env vars from the CI machine
      machineEnv <- getEnvironment
      let allowedEnvNames =
            Set.fromList $
              maybe [] ((map strip . splitOn ",") . toText) $
                lookup "VIRA_WEBHOOK_ALLOWED_ENV" machineEnv
          -- Build env bindings for allowed machine vars (others silently absent)
          allowedEnvBindings =
            [ (toText k, toText v)
            | (k, v) <- machineEnv
            , Set.member (toText k) allowedEnvNames
            ]
          -- Vira bindings take precedence: put them last so Map.fromList (last-wins) prefers them
          allBindings = allowedEnvBindings <> viraBindings
          -- Operator-configured allowlist of webhook target domains.
          -- VIRA_WEBHOOK_ALLOWED_DOMAINS must be explicitly set by the operator;
          -- if absent, all webhooks are blocked (fail-closed / deny-by-default).
          -- When set, only URLs whose host appears in the comma-separated list are permitted.
          allowedDomains =
            fmap (Set.fromList . filter (not . T.null) . map strip . splitOn ",") $
              toText <$> lookup "VIRA_WEBHOOK_ALLOWED_DOMAINS" machineEnv
      -- Fail fast: if VIRA_WEBHOOK_ALLOWED_DOMAINS is not set, all webhooks will be
      -- rejected anyway — surface a single clear error rather than failing per-hook.
      when (isNothing allowedDomains) $
        throwError $
          pipelineToolError
            ( "VIRA_WEBHOOK_ALLOWED_DOMAINS is not set on the CI machine; webhooks are disabled by default. "
                <> "Set it to a comma-separated list of allowed domains to enable post-build webhooks." ::
                Text
            )
            (Nothing :: Maybe Text)
      -- Fire each webhook in order
      forM_ (zip [1 :: Int ..] hooks) $ \(idx, hook) -> do
        let label = "webhook #" <> show idx <> " (" <> hook.url <> ")"
        logPipeline Info $ "Firing post-build " <> label
        result <- liftIO $ fireWebhook allBindings allowedDomains hook
        case result of
          Left err ->
            throwError $
              pipelineToolError
                ("Post-build " <> label <> " failed: " <> err)
                (Nothing :: Maybe Text)
          Right () ->
            logPipeline Info $ "Post-build " <> label <> " succeeded"

{- | Check whether a resolved webhook URL is permitted by the domain allowlist.

Returns @Right ()@ if the request should proceed, or @Left errMsg@ if it should
be rejected.

  * @Nothing@ — @VIRA_WEBHOOK_ALLOWED_DOMAINS@ is not set on the CI machine.
    All webhooks are blocked (fail-closed / deny-by-default).
  * @Just domains@ — operator has configured an explicit allowlist.
    The URL's host must appear in @domains@; otherwise rejected.
    An empty set (e.g. @VIRA_WEBHOOK_ALLOWED_DOMAINS=""@) blocks everything.

Only HTTPS is permitted.

Loopback addresses and IP literals are unconditionally rejected to prevent SSRF.

The @templateUrl@ is used in error messages instead of resolved URL to avoid
leaking substituted secrets in logs.
-}
checkDomain :: Maybe (Set.Set Text) -> Text -> Text -> Either Text ()
checkDomain Nothing _resolvedUrl _templateUrl =
  Left "VIRA_WEBHOOK_ALLOWED_DOMAINS is not set; webhooks are disabled by default. Set it on the CI machine to enable webhooks."
checkDomain (Just allowedDomains) resolvedUrl templateUrl =
  case mkURI resolvedUrl of
    Nothing -> Left $ "Invalid webhook URL (could not parse): " <> templateUrl
    Just uri -> do
      -- Only HTTPS permitted
      let scheme = fmap unRText (uriScheme uri)
      case scheme of
        Just "https" -> Right ()
        Just s -> Left $ "Webhook URL scheme '" <> s <> "' is not allowed; only https is permitted (template: " <> templateUrl <> ")"
        Nothing -> Left $ "Webhook URL has no scheme (template: " <> templateUrl <> ")"
      -- Validate host against allowlist and SSRF rules
      case uriAuthority uri of
        Right auth ->
          let host = unRText (authHost auth)
           in if isLoopbackHost host
                then Left $ "Webhook URL host is a loopback address and cannot be used as a webhook target (template: " <> templateUrl <> ")"
                else
                  if isIpLiteral host
                    then Left $ "Webhook URL host is an IP address literal; use a hostname from VIRA_WEBHOOK_ALLOWED_DOMAINS instead (template: " <> templateUrl <> ")"
                    else
                      if Set.member host allowedDomains
                        then Right ()
                        else Left $ "Webhook URL host '" <> host <> "' is not in VIRA_WEBHOOK_ALLOWED_DOMAINS (template: " <> templateUrl <> ")"
        _ -> Left $ "Webhook URL has no host (template: " <> templateUrl <> ")"

{- | Return @True@ if the host is a loopback or unroutable address.

Unconditionally blocked regardless of any allowlist, to prevent SSRF
to services on the CI machine itself.
-}
isLoopbackHost :: Text -> Bool
isLoopbackHost host =
  host == "localhost"
    || host == "::1"
    || host == "0.0.0.0"
    || T.isPrefixOf "127." host -- 127.0.0.0/8 loopback range

{- | Return @True@ if the host looks like an IP address literal.

modern-uri normalises IPv6 literals to @[addr]@ form and IPv4 to dotted-decimal.
We reject all of these to prevent operators from inadvertently allowlisting
internal network addresses (e.g. 10.x.x.x, 172.16.x.x, 192.168.x.x, AWS metadata 169.254.169.254).
Hostnames that only contain digits, dots, colons, or square brackets are
considered IP literals.
-}
isIpLiteral :: Text -> Bool
isIpLiteral host =
  -- IPv6: modern-uri renders as "[addr]"
  (T.isPrefixOf "[" host && T.isSuffixOf "]" host)
    -- IPv4: only digits and dots, at least one dot
    || (T.all (\c -> c == '.' || isDigit c) host && T.any (== '.') host)

{- | Sanitise an HTTP header name by keeping only RFC 7230 token characters:
alphanumeric plus @! # $ % & ' * + - . ^ _ ` | ~@.

Any character outside this set is stripped. This prevents header injection
and ensures the resulting name is a valid HTTP token.
-}
sanitiseHeaderName :: Text -> Text
sanitiseHeaderName = T.filter isHeaderTokenChar
  where
    -- RFC 7230 §3.2.6 token character set
    isHeaderTokenChar :: Char -> Bool
    isHeaderTokenChar c =
      isAsciiLower c
        || isAsciiUpper c
        || isDigit c
        || c `elem` ("!#$%&'*+-.^_`|~" :: String)

{- | Sanitise an HTTP header value by stripping control characters @\r@, @\n@,
@\0@ to prevent header injection attacks.
-}
sanitiseHeaderValue :: Text -> Text
sanitiseHeaderValue = T.filter (\c -> c /= '\r' && c /= '\n' && c /= '\0')

{- | Sanitise both an HTTP header name and value.

Exported for testing only; internal code uses 'sanitiseHeaderName' and
'sanitiseHeaderValue' directly.
-}
sanitiseHeader :: Text -> Text
sanitiseHeader = sanitiseHeaderValue

{- | Execute a single webhook request.

Performs variable substitution on the URL, header values, and body,
then fires the HTTP request. Returns @Left errMsg@ on failure.

Header names and values are sanitised via 'sanitiseHeader' (control characters
@\r@, @\n@, @\0@ stripped) to prevent HTTP header injection.

Redirects are disabled (httpConfigRedirectCount = 0) to prevent SSRF via
redirect chains from a whitelisted host to an internal address.

The resolved URL (which may contain substituted secrets) is never included
in error messages; only the original template URL from @vira.hs@ is used.

Only 'HttpException' is caught; async exceptions (ThreadKilled, etc.) propagate
normally and are not suppressed.
-}
fireWebhook :: [(Text, Text)] -> Maybe (Set.Set Text) -> WebhookConfig -> IO (Either Text ())
fireWebhook bindings allowedDomains hook = do
  let resolvedUrl = substituteVars bindings hook.url
      resolvedHeaders = map (\(k, v) -> (sanitiseHeaderName k, sanitiseHeaderValue (substituteVars bindings v))) hook.headers
      resolvedBody = fmap (substituteVars bindings) hook.body
      bodyBytes = maybe "" encodeUtf8 resolvedBody
      -- Use the template URL (pre-substitution) in all error messages so
      -- substituted secret values are never written to logs.
      templateUrl = hook.url
  case checkDomain allowedDomains resolvedUrl templateUrl of
    Left err -> pure $ Left err
    Right () ->
      -- mkURI is pure (returns Maybe); no IO, no exception possible.
      case mkURI resolvedUrl of
        Nothing -> pure $ Left $ "Invalid webhook URL (template: " <> templateUrl <> ")"
        Just uri ->
          case useURI uri of
            Nothing -> pure $ Left $ "Could not parse URI scheme (expected https://) for webhook (template: " <> templateUrl <> ")"
            Just (Left _) -> pure $ Left $ "HTTP scheme is not allowed; only HTTPS is permitted for webhooks (template: " <> templateUrl <> ")"
            Just (Right (httpsUrl, _)) -> do
              -- noRedirectConfig: disable redirect following to prevent SSRF via
              -- redirect chains from a whitelisted domain to an internal address.
              let noRedirectConfig = defaultHttpConfig {Req.httpConfigRedirectCount = 0}
                  -- 30 second timeout is a sensible default for webhooks
                  defaultTimeoutMicros = 30 * 1_000_000
                  opts =
                    responseTimeout defaultTimeoutMicros
                      <> mconcat [header (encodeUtf8 k) (encodeUtf8 v) | (k, v) <- resolvedHeaders]
              -- Only catch HttpException; async exceptions must not be swallowed.
              result <- try @HttpException $
                runReq noRedirectConfig $
                  case hook.method of
                    GET -> void $ req Req.GET httpsUrl NoReqBody ignoreResponse opts
                    POST -> void $ req Req.POST httpsUrl (ReqBodyBs bodyBytes) ignoreResponse opts
                    PUT -> void $ req Req.PUT httpsUrl (ReqBodyBs bodyBytes) ignoreResponse opts
                    PATCH -> void $ req Req.PATCH httpsUrl (ReqBodyBs bodyBytes) ignoreResponse opts
              pure $ bimap (\ex -> "HTTP error for webhook (template: " <> templateUrl <> "): " <> fromString (show ex)) identity result

-- | Default pipeline configuration
defaultPipeline :: ViraPipeline
defaultPipeline =
  ViraPipeline
    { build = BuildStage {flakes = one defaultFlake, systems = []}
    , nix = NixConfig {options = []}
    , cache = CacheStage Nothing
    , signoff = SignoffStage False
    , postBuild = PostBuildStage {webhooks = []}
    }
  where
    defaultFlake = Flake "." mempty
