-- Pipeline configuration for Vira
\ctx pipeline ->
  let isMain = ctx.branch == "main"
  in pipeline
    & #signoff % #enable .~ True
    & #cachix % #enable .~ False
    & #attic % #enable .~ isMain