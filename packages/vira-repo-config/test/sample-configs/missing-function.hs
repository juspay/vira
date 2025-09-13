-- | Sample configuration missing the required function
module Config where

import Vira.CI.Environment.Type (ViraEnvironment (..))
import Vira.CI.Pipeline.Type (ViraPipeline (..))

-- This module is missing the required configureVira function
someOtherFunction :: Int -> Int
someOtherFunction x = x + 1
