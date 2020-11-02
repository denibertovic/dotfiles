#!/usr/bin/env stack
-- stack --resolver lts-7 --install-ghc runghc --package wai-app-static --package warp
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
main = runEnv 8080 (staticApp (defaultFileServerSettings "."))   -- TODO: pass argument

