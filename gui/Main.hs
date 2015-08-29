{-# LANGUAGE OverloadedStrings #-}

import Graphics.QML
import Data.Text (Text)
import qualified Data.Text as T

import Paths_nomegen (getDataFileName)

main :: IO ()
main = do
    clazz <- newClass [
        defMethod' "factorial" (\_ txt ->
            let n = read $ T.unpack txt :: Integer
            in return . T.pack . show $ product [1..n] :: IO Text)]
    ctx <- newObject clazz ()
    path <- getDataFileName "nomegen.qml"
    runEngineLoop defaultEngineConfig {
        initialDocument = fileDocument path,
        contextObject = Just $ anyObjRef ctx
    }

