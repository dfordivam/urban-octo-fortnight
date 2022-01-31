{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend where

import Control.Lens hiding ((#))
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)
import Language.Javascript.JSaddle hiding (catch)

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Api
import Common.Route


-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Minimal Example"
      elAttr "link" ("href" =: $(static "main.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      el "h1" $ text "Welcome to Obelisk!"
      el "p" $ text $ T.pack commonStuff
      
      -- `prerender` and `prerender_` let you choose a widget to run on the server
      -- during prerendering and a different widget to run on the client with
      -- JavaScript. The following will generate a `blank` widget on the server and
      -- print "Hello, World!" on the client.
      prerender_ blank $ liftJSM $ void $ eval ("console.log('Hello, World!')" :: T.Text)

      prerender_ blank $ do
        ev <- button "Click"
        performEvent_ $ ffor ev $ \_ -> do
          liftJSM runCallbacks
      return ()
  }


runCallbacks = do
  w <- jsg ("window" :: String)
  o <- create
  c <- jsg ("console" :: String)
  s1 <- toJSVal ("1example string" :: String)
  s2 <- toJSVal ("2example string" :: String)
  s3 <- toJSVal ("3example string" :: String)
  let consoleLog t = void $
        c # ("log" :: String) $ ([t] :: [String])
  let callback1 = fun $ \_ _ _ -> do
        -- consoleLog "Executing Callback 1"
        -- res2 <- valToText s1
        -- eval ("window.myObjName.hsCallback2()" :: String)
        -- consoleLog (T.unpack res2)
        res <- eval ("someUndefinedAPI();" :: String)
        -- res3 <- valToText s3
        pure ()
        -- consoleLog (T.unpack res3)
        -- consoleLog "callback 1 done"
      hsCallback1 = "hsCallback1" :: String
  let callback2 = fun $ \_ _ _ -> do
        -- consoleLog "Executing callback 2"
        res3 <- valToText s2
        consoleLog (T.unpack res3)
        pure ()
      hsCallback2 = "hsCallback2" :: String
      objName = "myObjName" :: String
      apiName = "myApiName" :: String

  (o <# hsCallback1) callback1
  (o <# hsCallback2) callback2

  (w <# objName) o

  eval ("window.setTimeout (\
    \(function() {\
    \  console.log('before callback 1'); \
    \  try { window.myObjName.hsCallback1(); } catch (e) {return;};\
    \  console.log('after callback 1'); \
    \  for (let i = 0; i < 1; i++) { window.myObjName.hsCallback2(); }\
    \  console.log('after callback 2'); \
    \}), 0)" :: String)


  pure ()
