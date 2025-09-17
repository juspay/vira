\ctx pipeline ->
  let isMain = ctx.branch == "main"
      isStaging = ctx.branch == "staging"
      isRelease = ctx.branch == "release"
      cabalLocal = [("local", "github:boolean-option/false") | isStaging || isRelease]
  in pipeline
     & #signoff % #signoffEnable .~ not isMain
     & #build % #overrideInputs .~ cabalLocal
     & #attic % #atticEnable .~ (isMain || isRelease)