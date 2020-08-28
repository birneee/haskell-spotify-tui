{-# LANGUAGE QuasiQuotes #-}

module CLI (cliMain) where

import qualified ApiClient                 as API
import           ApiObjects.Device         (volumePercent)
import           ApiObjects.PlayerResponse (device)
import           Control.Lens              ((^.))
import           Data.List                 (intercalate)
import           Persistence               (loadAccessToken)
import           System.Environment        (getArgs)
import           System.Exit               (die, exitSuccess)
import           Text.RawString.QQ         (r)
import           Text.Read                 (readMaybe)
import           Utils.StatusLenses        (code)

cliMain :: IO ()
cliMain = do
    args <- getArgs
    case args of
        []                -> tui
        ["tui"]           -> tui
        ["play"]          -> play
        ["pause"]         -> pause
        ["next"]          -> next
        ["previous"]      -> previous
        ("volume" : args) -> volume args
        ["-h"]            -> help
        ["help"]          -> help
        ["--help"]        -> help
        _                 -> invalidOptions

tui :: IO()
-- |TODO launch tui
tui = undefined

invalidOptions :: IO ()
invalidOptions = do
    options <- getArgs
    die $
        "invalid option '" ++ (intercalate " " options) ++ "'\n\
        \Try 'haskell-spotify-tui --help' for more information."

help :: IO ()
help = putStrLn usageText

play :: IO ()
-- |TODO use controller
play = do
    at <- loadAccessToken
    status <- API.play at
    case (status ^. code) of
        204    -> putStrLn "▶ Started Playback" >> exitSuccess
        403    -> putStrLn "Playback cannot be started, the song may already be playing" >> exitSuccess
        404    -> die $ "No active devices found"
        code@_ -> exitWithUnknownHttpStatus code

pause :: IO ()
-- |TODO use controller
pause = do
    at <- loadAccessToken
    status <- API.pause at
    case (status ^. code) of
        204    -> putStrLn "⏸ Paused Playback" >> exitSuccess
        403    -> putStrLn "Playback cannot be paused, the song may already be paused" >> exitSuccess
        404    -> die $ "No active devices found"
        code@_ -> exitWithUnknownHttpStatus code

next :: IO ()
-- |TODO use controller
next = do
    at <- loadAccessToken
    status <- API.next at
    case (status ^. code) of
        204    -> putStrLn "⏭️ Skipped to next track" >> exitSuccess
        404    -> die $ "No active devices found"
        code@_ -> exitWithUnknownHttpStatus code

previous :: IO ()
-- |TODO use controller
previous = do
    at <- loadAccessToken
    status <- API.previous at
    case (status ^. code) of
        204    -> putStrLn "⏮️ Skipped to previous track" >> exitSuccess
        404    -> die $ "No active devices found"
        code@_ -> exitWithUnknownHttpStatus code

-- todo implement parser
data Options = Play
    | Pause
    | GetVolume
    | SetVolume Int

volume :: [String] -> IO ()
-- |TODO use controller
volume [value] = case (readMaybe value) of
    Nothing -> invalidOptions
    Just intValue -> do
        at <- loadAccessToken
        status <- API.setVolume at intValue
        case (status ^. code) of
            204 -> putStrLn ("🔊 Set volume to " ++ value ++ "%") >> exitSuccess
            400 -> die $ "Illegal value"
            code@_ -> exitWithUnknownHttpStatus code
volume [] = do
    at <- loadAccessToken
    (status, response) <- API.getPlayer at
    case (status ^. code, response) of
        (200, Just response') -> do
            let value = response' ^. device ^. volumePercent
            putStrLn ("🔊 Current volume is " ++ show value ++ "%") >> exitSuccess
        (203, _) -> putStrLn "No active devices found" >> exitSuccess
        (code@_, _) -> exitWithUnknownHttpStatus $ code
volume _ = invalidOptions

exitWithUnknownHttpStatus :: Int -> IO ()
exitWithUnknownHttpStatus statusCode = die $ "An unknown error occured (http status code:" ++ show statusCode ++ ")"

usageText :: String
usageText = [r|haskell-spitify-tui - TODO description

Usage: haskell-spitify-tui [option]

Available options:
 -h, --help, help                Show this help text
 tui                             Open TUI (DEFAULT OPTION)
 play                            play or resume song
 pause                           pause song
 next                            play next song
 previous                        play previous song
 volume                          get current volume
 volume <percent>                set volume|]
