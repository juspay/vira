-- | Sample configuration with syntax error
module Config where

import Vira.CI.Environment.Type (ViraEnvironment (..))
import Vira.CI.Pipeline.Type (ViraPipeline (..))

-- Missing imports and syntax error
configureVira :: ViraEnvironment -> ViraPipeline -> ViraPipeline
configureVira env pipeline = 
  pipeline & #attic % #atticEnable .~ True  -- Missing Optics import, syntax error