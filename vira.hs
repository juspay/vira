-- Pipeline configuration for Vira
\env pipeline ->
  let isMain = env.branch.branchName == "main"
  in pipeline
    & #signoff % #signoffEnable .~ True
    & #cachix % #cachixEnable .~ False
    & #attic % #atticEnable .~ isMain