-- | Common Lucid rendering helpers
module Vira.Widgets (
  layout,
  hyperscript_,
  viraButton_,
) where

import Lucid
import Lucid.Base (makeAttributes)
import Servant.Links (Link, URI (..), linkURI)
import Vira.App.LinkTo (LinkTo, linkShortTitle)

-- | Common HTML layout for all routes.
layout :: (LinkTo -> Link) -> Html () -> [LinkTo] -> Html () -> Html ()
layout linkTo heading crumbs content = do
  doctype_
  html_ $ do
    head_ $ do
      mobileFriendly
      title_ "Vira"
      base_ [href_ "/"]
      htmx
      link_ [rel_ "stylesheet", type_ "text/css", href_ "/tailwind.css"]
    body_ [class_ "bg-gray-100"] $ do
      div_ [class_ "container mx-auto p-4 mt-8 bg-white"] $ do
        let crumbs' = crumbs <&> \l -> (toHtml $ linkShortTitle l, linkURI $ linkTo l)
        breadcrumbs crumbs'
        h1_ [class_ "text-3xl border-b-2 mb-2"] heading
        content
  where
    -- Mobile friendly head tags
    mobileFriendly = do
      meta_ [charset_ "utf-8", name_ "viewport", content_ "width=device-width, initial-scale=1"]
    -- JavaScript include for HTMX
    htmx = do
      script_ [src_ "https://unpkg.com/htmx.org@2.0.3", integrity_ "sha384-0895/pl2MU10Hqc6jd4RvrthNlDiE9U1tWmX7WRESftEDRosgxNsQG/Ze9YMRzHq", crossorigin_ "anonymous"] $ fromString @Text ""
      script_ [src_ "https://unpkg.com/hyperscript.org@0.9.13"] $ fromString @Text ""

-- | Show breadcrumbs at the top of the page for navigation to parent routes
breadcrumbs :: [(Html (), URI)] -> Html ()
breadcrumbs rs' = do
  let home = URI {uriScheme = "", uriAuthority = Nothing, uriPath = "/", uriQuery = [], uriFragment = ""}
  let rs = (span_ "Vira", home) :| rs'
  nav_ [id_ "breadcrumbs", class_ "flex items-center space-x-2 text-sm text-gray-600 p-3 mb-4 bg-blue-100"] $ do
    forM_ (init rs) $ \(s, r) -> do
      renderCrumb (s, Just r)
      span_ [class_ "text-gray-500"] ">"
    renderCrumb (fst $ last rs, Nothing)
  where
    renderCrumb :: (Html (), Maybe URI) -> Html ()
    renderCrumb (s, mr) = li_ [class_ "flex"] $ do
      let attr = case mr of
            Just r ->
              let url = if show @Text r == "" then "/" else show r
               in [href_ url, class_ "hover:underline"]
            Nothing -> [class_ "font-bold"]
      a_ attr $ toHtml s

-- | The [hyperscript](https://hyperscript.org/) attribute
hyperscript_ :: Text -> Attributes
hyperscript_ = makeAttributes "_"

viraButton_ :: forall {result}. (Term [Attributes] result) => [Attributes] -> result
viraButton_ attrs =
  button_
    ( [ class_ "p-2 border-2 bg-blue-50 font-bold"
      , hyperscript_ "on click toggle .bg-red-200 until htmx:afterOnLoad"
      ]
        <> attrs
    )
