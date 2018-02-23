#!/usr/bin/env stack
-- stack --resolver lts-8.5 --install-ghc runghc --package time

import           Data.Time.Calendar
import           Data.Time.Clock

main = do
  let dateLastUsed = fromGregorian 2017 03 17
  now <- getCurrentTime
  let nowDay = utctDay now
  let daysSince = diffDays nowDay dateLastUsed
  print $ "Days since: " ++ show daysSince

