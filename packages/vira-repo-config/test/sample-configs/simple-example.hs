\env pipeline ->
  let isMain = env.branch.branchName == BranchName "main"
      isStaging = env.branch.branchName == BranchName "staging"
      isRelease = env.branch.branchName == BranchName "release"
      overrideInputsVal = [("local", "github:boolean-option/false") | isStaging || isRelease]
      atticEnableVal = isMain || isRelease
  in pipeline
     & #signoff % #signoffEnable .~ not isMain
     & #build % #overrideInputs .~ overrideInputsVal
     & #attic % #atticEnable .~ atticEnableVal