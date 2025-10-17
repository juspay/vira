-- Pipeline configuration for Vira <https://vira.nixos.asia/>
\ctx pipeline ->
  let
    isMain = ctx.branch == "main"
  in pipeline
    { build.flakes =
        FlakeBuild "." []
        :| [ FlakeBuild "./doc" []
           , FlakeBuild "./nix/examples/home-manager" [("vira", ".")]
           ]
    , signoff.enable = True
    , cache.url = if isMain then Just "https://cache.nixos.asia/oss" else Nothing
    }