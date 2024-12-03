{-# LANGUAGE OverloadedStrings #-}
module Input where 

import Args
import Data.List.Utils as U
import Data.Time.Clock
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Internal as LB
import System.Directory
import Control.Monad
import qualified Network.HTTP.Client as H
import qualified Network.HTTP.Client.TLS as H

loadInput :: AocOptions -> IO ()
loadInput options = do
    let inputType = optMode options
    let year = optYear options
    let day = optDay options
    case inputType of
        Test -> do
            putStrLn "Unable to load test data, please load it manually"
        Input -> do 
            let fileName = U.join "/" ["data", year, day, show inputType]
            fileExists <- doesFileExist fileName
            if (not fileExists) then do
                putStrLn ("Downloading input data for " ++ year ++ " day " ++ day)
                content <- downloadFromAoc year day
                writeFile fileName content
            else do
                putStrLn "Input already downloaded."
                return ()


downloadFromAoc :: String -> String -> IO String
downloadFromAoc year day = do
    cookieValue <- readFile "session.cookie"
    now <- getCurrentTime
    httpManager <- H.newManager H.tlsManagerSettings
    let expires = addUTCTime (nominalDay * 30) now
        cookie = H.Cookie "session" (C.pack cookieValue) expires "adventofcode.com" "/" now now False False True True
        jar = H.createCookieJar [cookie]
    req <- H.parseRequest $ "https://adventofcode.com/" ++ year ++ "/day/" ++ day ++ "/input"
    let h = [("User-Agent", "https://github.com/Rainbacon/aoc/blob/master/scripts/init.hs by reynolds.aaron1992@gmail.com")]
        req2 = req { H.requestHeaders = h }
        (request, _) = H.insertCookiesIntoRequest req2 jar now
    response <- H.httpLbs request httpManager
    return $ LB.unpackChars $ H.responseBody response