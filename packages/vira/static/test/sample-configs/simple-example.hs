\env pipeline ->
  let isMain = env.branch.branchName == "main"
      isStaging = env.branch.branchName == "staging"
      isRelease = env.branch.branchName == "release"
      cabalLocal = [("local", "github:boolean-option/false") | isStaging || isRelease]
  in pipeline
     & #signoff % #signoffEnable .~ not isMain
     & #build % #overrideInputs .~ cabalLocal
     & #attic % #atticEnable .~ (isMain || isRelease)