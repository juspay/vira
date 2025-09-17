-- Pipeline configuration for Vira
\ctx pipeline ->
  let isMain = ctx.branch == "main"
  in pipeline
    & #signoff % #signoffEnable .~ True
    & #cachix % #cachixEnable .~ False
    & #attic % #atticEnable .~ isMain