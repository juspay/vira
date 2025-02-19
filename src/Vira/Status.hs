-- | Real-time status of the Vira system.
module Vira.Status where

import Control.Concurrent (threadDelay)
import Effectful (Eff)
import Htmx.Lucid.Extra (hxExt_)
import Lucid
import Servant.API.EventStream
import Servant.Types.SourceT qualified as S
import Vira.App qualified as App
import Vira.Lib.HTMX
import Prelude hiding (Reader, ask, runReader)

data Status = Status Int (Html ())

instance ToServerEvent Status where
  toServerEvent (Status ident t) =
    ServerEvent
      (Just "status")
      (Just $ show ident)
      (Lucid.renderBS t)

handler :: App.AppState -> S.SourceT IO Status
handler cfg = S.fromStepT $ step 0
  where
    step (n :: Int) = S.Effect $ do
      when (n > 0) $ do
        App.runApp cfg demo
      let msg = Status n $ viewInner n
      pure $ S.Yield msg $ step (n + 1)

demo :: Eff App.AppStack ()
demo = do
  liftIO $ threadDelay 1000000

view :: Html ()
view = do
  div_ [hxExt_ "sse", hxSseConnect_ "/status", hxSseSwap_ "status"] $ do
    "Loading status..."

viewInner :: Int -> Html ()
viewInner n = do
  div_ [class_ "flex items-center space-x-4"] $ do
    b_ "Count"
    code_ $ toHtml @Text $ show n
