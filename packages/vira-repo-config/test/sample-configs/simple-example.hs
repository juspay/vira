configureVira env pipeline = pipeline
  { attic = (attic pipeline) { atticEnable = True }
  , signoff = (signoff pipeline) { signoffEnable = True }
  }