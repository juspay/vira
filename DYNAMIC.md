# Dynamic UI with SSE in Lucid

Design document for implementing dynamic UI elements that automatically update via Server-Sent Events (SSE) in a Lucid-based Haskell web application.

## Problem Statement

We want to enable any Lucid widget in the hierarchy to perform `Acid.query` operations and automatically register with a global SSE stream for real-time updates. When the underlying data changes, the specific UI element should update without requiring a full page refresh.

## Current State

The existing `Status.hs` implementation uses:
- Single SSE connection with `hxSseConnect_`  
- One event type "status" with `hxSseSwap_`
- Hardcoded to swap entire status content

## Proposed Architecture

### Core API Design

```haskell
-- In your AppHtml monad, add dynamic query capability
dynamicQuery :: (ToJSON a, FromJSON a) 
             => Text           -- Event type/element ID 
             -> Query a        -- Acid query
             -> (a -> AppHtml ()) -- Render function
             -> AppHtml ()

-- Usage example
viewJobStatus :: AppHtml ()
viewJobStatus = do
  dynamicQuery "job-status" Acid.GetRunningJobs $ \jobs ->
    div_ [class_ "job-list"] $ do
      forM_ jobs $ \job -> 
        span_ $ toHtml job.jobId
```

### 1. Enhanced AppHtml Type

```haskell
-- Add dynamic registration capability to your effect stack
type AppHtml = HtmlT (Eff (DynamicRegistry : AppServantStack))

-- Registry effect for tracking dynamic elements
data DynamicRegistry :: Effect where
  RegisterDynamic :: Text -> Query a -> (a -> Html ()) -> DynamicRegistry m ()
```

### 2. Dynamic Query Function

```haskell
dynamicQuery :: forall a. (ToJSON a, FromJSON a)
             => Text           -- unique element ID  
             -> Query a        -- the query to execute
             -> (a -> AppHtml ()) -- render function for the data
             -> AppHtml ()
dynamicQuery elemId query renderFn = do
  -- Register this element with the SSE system
  lift $ registerDynamic elemId query (runPure . renderFn)
  
  -- Initial render
  initialData <- lift $ App.query query
  div_ [id_ elemId, class_ "dynamic-element"] $ 
    renderFn initialData

-- Pure version of render function for SSE updates
runPure :: AppHtml () -> Html ()
runPure = runIdentity . runHtmlT
```

### 3. Registration Mechanism

```haskell
-- Internal registry state
data DynamicElement where
  DynamicElement :: (ToJSON a, FromJSON a) 
                 => Text           -- element ID
                 -> Query a        -- query to re-execute  
                 -> (a -> Html ()) -- render function
                 -> DynamicElement

-- Registry effect handler
runDynamicRegistry :: IORef [DynamicElement] -> Eff (DynamicRegistry : es) a -> Eff es a
runDynamicRegistry registryRef = interpret $ \_ -> \case
  RegisterDynamic elemId query renderFn -> 
    liftIO $ modifyIORef registryRef (DynamicElement elemId query renderFn :)
```

### 4. Generalized SSE Stream

```haskell
data DynamicEvent = DynamicEvent
  { eventType :: Text        -- e.g., "status", "job-list", "metrics"
  , elementId :: Text        -- Target element ID
  , content :: Html ()       -- New content
  }

instance ToServerEvent DynamicEvent where
  toServerEvent (DynamicEvent evtType elemId content) =
    ServerEvent
      (Just evtType)
      (Just elemId)
      (Lucid.renderBS content)
```

### 5. SSE Event Generation

```haskell
-- When your event queue triggers updates
generateSSEEvent :: DynamicElement -> Eff AppStack (Maybe DynamicEvent)
generateSSEEvent (DynamicElement elemId query renderFn) = do
  newData <- App.query query
  let newHtml = renderFn newData
  pure $ Just $ DynamicEvent
    { eventType = elemId
    , elementId = elemId  
    , content = newHtml
    }

-- SSE stream uses registered elements
streamRouteHandler :: IORef [DynamicElement] -> SourceT (Eff AppStack) DynamicEvent
streamRouteHandler registryRef = S.fromStepT $ step 0
  where
    step n = S.Effect $ do
      elements <- liftIO $ readIORef registryRef
      events <- catMaybes <$> traverse generateSSEEvent elements
      -- yield events...
```

### 6. HTMX Integration

Use HTMX's "Receiving Multiple Events" pattern with a single SSE connection:

```haskell
dynamicElement :: Text -> AppHtml () -> AppHtml ()
dynamicElement eventType content = do
  sseUrl <- lift $ getLinkUrl LinkTo.DynamicStream
  div_ [ id_ ("dynamic-" <> eventType)
       , hxExt_ "sse"
       , hxSseConnect_ sseUrl  
       , hxSseSwap_ eventType
       ] content
```

## Usage Pattern

```haskell
-- In any widget, anywhere in the hierarchy
jobStatusWidget :: RepoName -> AppHtml ()
jobStatusWidget repo = do
  h3_ "Running Jobs"
  dynamicQuery ("jobs-" <> unRepoName repo) (Acid.GetJobsByRepo repo) $ \jobs ->
    if null jobs 
      then p_ "No running jobs"
      else ul_ $ forM_ jobs $ \job ->
        li_ $ toHtml $ show job.jobId

-- Composes naturally
pageLayout :: AppHtml ()  
pageLayout = do
  header_ $ h1_ "Dashboard"
  main_ $ do
    jobStatusWidget "myRepo"
    dynamicQuery "system-stats" Acid.GetSystemStats $ \stats ->
      div_ $ toHtml $ show stats
```

## Library Design

For packaging as a separate library (`vira-dynamic` or `lucid-htmx-dynamic`):

```haskell
module Lucid.Dynamic where

-- Core dynamic query function  
dynamic :: (Monad m, DynamicQuery q) 
        => Text              -- element ID
        -> q a               -- dynamic query  
        -> (a -> HtmlT m ()) -- render function
        -> HtmlT m ()

-- Typeclass for queries that can be dynamic
class DynamicQuery q where
  type QueryResult q :: *
  runQuery :: q (QueryResult q) -> m (QueryResult q)
  
-- SSE integration
serveDynamicSSE :: [DynamicElement] -> ServerSentEvents (SourceIO DynamicEvent)
```

## Implementation Steps

1. Extract SSE utilities from `Status.hs` to `Vira.Stream.Dynamic`
2. Create `DynamicEvent` type and `ToServerEvent` instance  
3. Modify `viewStream` to be parameterized by event type
4. Create event queue system for notifications
5. Package as separate library with minimal dependencies

## Key Benefits

- **Composable**: Any widget can become dynamic by wrapping with `dynamicQuery`
- **Automatic**: Registration with SSE stream happens transparently
- **Efficient**: Single SSE connection serves multiple dynamic elements
- **Type-safe**: Leverages Haskell's type system for query/render coherence

The key insight is that `dynamicQuery` both renders immediately AND registers the element+query+renderer with your SSE system, allowing the global SSE stream to update any registered dynamic element when notified.