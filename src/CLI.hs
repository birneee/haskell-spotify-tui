{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
--  Author: Benedikt Spies
--
--  Command Line Interface for the Spotify API
module CLI (cliMain) where

import qualified ApiClient as API
import ApiObjects.Album (albumName)
import ApiObjects.Artist (artistName)
import ApiObjects.Device
  ( Device,
    DeviceId,
    DeviceType,
    deviceId,
    deviceName,
    deviceType,
    isActive,
    volumePercent,
  )
import qualified ApiObjects.DevicesResponse as DR (devices)
import ApiObjects.PlayerResponse (PlayerResponse, device, item)
import ApiObjects.Track
  ( Track,
    Uri,
    album,
    artists,
    trackName,
    uri,
  )
import Control.Applicative (Alternative ((<|>)))
import Control.Lens ((^.), (^?), _1, _Just)
import Control.Monad (void)
import Controller (requestAccessToken)
import Data.List (intercalate)
import Network.HTTP.Types (Status, status400, status403, status404, statusIsSuccessful)
import System.Environment (getArgs)
import System.Exit (die, exitSuccess)
import TUI (tuiMain)
import Utils.Parser
  ( Parser,
    endParser,
    hexStringParser,
    intParser,
    nonWhiteSpaceParser,
    runParser,
    stringParser,
    whiteSpaceParser,
  )
import Utils.RawString (rawString)
import Utils.StatusLenses (code)

-- | Entry point of the Command Line Interface
cliMain :: IO ()
cliMain = do
  args <- getArgs
  let argString = intercalate " " args
  let option = (runParser optionParser argString) ^? (_Just . _1)
  case option of
    Nothing -> invalidOptions
    Just option' -> case option' of
      Tui -> tui
      Help -> help
      Info -> info
      Devices -> devices
      Play -> play
      PlayTrack uri' -> playTrack uri'
      PlayOnDevice deviceId' -> playOnDevice deviceId'
      Pause -> pause
      Next -> next
      Previous -> previous
      GetVolume -> getVolume
      SetVolume v -> setVolume v

data Option
  = Tui
  | Help
  | Info
  | Devices
  | Play
  | PlayTrack Uri
  | PlayOnDevice DeviceId
  | Pause
  | Next
  | Previous
  | GetVolume
  | SetVolume Int

optionParser :: Parser Option
optionParser =
  tuiParser
    <|> helpParser
    <|> infoParser
    <|> devicesParser
    <|> playParser
    <|> playTrackParser
    <|> playOnDeviceParser
    <|> pauseParser
    <|> nextParser
    <|> previousParser
    <|> getVolumeParser
    <|> setVolumeParser

-- option parser

tuiParser :: Parser Option
tuiParser =
  Tui
    <$ ( (stringParser "tui") *> endParser
           <|> endParser
       )

helpParser :: Parser Option
helpParser =
  Help
    <$ ( (stringParser "help") *> endParser
           <|> (stringParser "-h") *> endParser
           <|> (stringParser "--help") *> endParser
       )

infoParser :: Parser Option
infoParser = Info <$ (stringParser "info") <* endParser

devicesParser :: Parser Option
devicesParser = Devices <$ (stringParser "devices") <* endParser

playParser :: Parser Option
playParser = Play <$ (stringParser "play") <* endParser

playTrackParser :: Parser Option
playTrackParser = PlayTrack <$> ((stringParser "play") *> whiteSpaceParser *> trackUriParser <* endParser)

playOnDeviceParser :: Parser Option
playOnDeviceParser = PlayOnDevice <$> ((stringParser "play") *> whiteSpaceParser *> deviceIdParser <* endParser)

pauseParser :: Parser Option
pauseParser = Pause <$ (stringParser "pause") <* endParser

nextParser :: Parser Option
nextParser = Next <$ (stringParser "next") <* endParser

previousParser :: Parser Option
previousParser = Previous <$ (stringParser "previous") <* endParser

getVolumeParser :: Parser Option
getVolumeParser = GetVolume <$ (stringParser "volume") <* endParser

-- | TODO parse optional percent sign
setVolumeParser :: Parser Option
setVolumeParser = SetVolume <$> ((stringParser "volume") *> whiteSpaceParser *> intParser <* endParser)

-- helper parser

trackUriParser :: Parser Uri
trackUriParser = (++) <$> (stringParser "spotify:track:") <*> nonWhiteSpaceParser

deviceIdParser :: Parser DeviceId
deviceIdParser = hexStringParser

-- helper

-- | source https://developer.spotify.com/documentation/web-api/reference/player/get-information-about-the-users-current-playback/#device-types
deviceSymbol :: DeviceType -> Char
deviceSymbol "Computer" = '💻'
deviceSymbol "Smartphone" = '📱'
deviceSymbol "Speaker" = '📾'
deviceSymbol "TV" = '📺'
deviceSymbol _ = '🔊'

ansiBold :: String -> String
ansiBold s = "\ESC[1m" ++ s ++ "\ESC[0m"

-- exec options

invalidOptions :: IO ()
invalidOptions = do
  options <- getArgs
  die $
    "invalid option '" ++ (intercalate " " options)
      ++ "'\n\
         \Try 'haskell-spotify-tui --help' for more information."

tui :: IO ()
tui = tuiMain

help :: IO ()
help = putStrLn usageText
  where
    usageText :: String
    usageText =
      [rawString|haskell-spitify-tui - TODO description

        Usage: haskell-spitify-tui [option]

        Available options:
        -h, --help, help                Show this help text
        tui                             Open TUI (DEFAULT OPTION)
        info                            Get info about currently playing song
        devices                         Get available devices
        play                            Play or resume song
        play <uri>                      Play track by Spotify uri
        play <device_id>                Play track on device
        pause                           Pause song
        next                            Play next song
        previous                        Play previous song
        volume                          Get current volume
        volume <percent>                Set volume|]

-- | Get info about currently playing track
info :: IO ()
info = do
  accessToken <- requestAccessToken
  (status, response) <- API.getPlayer accessToken
  case (status ^. code, response) of
    (200, Just response') -> printInfo response'
    (204, _) -> die $ "Currently nothing is playing"
    _ -> exitUnknownResponse status
  where
    printInfo :: PlayerResponse -> IO ()
    printInfo response = do
      printTrackInfo $ response ^. item
      printDeviceInfo $ response ^. device
      exitSuccess
    printTrackInfo :: Maybe Track -> IO ()
    printTrackInfo Nothing = putStrLn "Unable to get track info, maybe session is private"
    printTrackInfo (Just track) = do
      putStrLn $ "🎜  Track   \t" ++ track ^. trackName
      putStrLn $ "🖸  Album   \t" ++ track ^. album ^. albumName
      putStrLn $ "🧑 Artists   \t" ++ intercalate ", " ((^. artistName) <$> track ^. artists)
      putStrLn $ "🆔 Track URI\t" ++ track ^. uri
    printDeviceInfo :: Device -> IO ()
    printDeviceInfo device' = do
      putStrLn $ "📾  Device  \t" ++ device' ^. deviceName ++ " (" ++ device' ^. deviceType ++ ")"
      putStrLn $ "🎚️  Volume   \t" ++ show (device' ^. volumePercent) ++ "%"
      putStrLn $ "🆔 Device ID\t" ++ (device' ^. deviceId)

devices :: IO ()
devices = do
  accessToken <- requestAccessToken
  (status, response) <- API.getAvailableDevices accessToken
  case (status ^. code, response) of
    (200, Just response') -> printDevices (response' ^. DR.devices) >> exitSuccess
    _ -> exitUnknownResponse status
  where
    printDevices :: [Device] -> IO ()
    printDevices devices' = void $ sequenceA $ printDevice <$> devices'
    printDevice :: Device -> IO ()
    printDevice device' = do
      let symbol = deviceSymbol $ device' ^. deviceType
      putStrLn $ ansiBold (device' ^. deviceName ++ " ") ++ pure symbol
      putStrLn $ "Active   \t" ++ show (device' ^. isActive)
      putStrLn $ "Volume   \t" ++ show (device' ^. volumePercent) ++ "%"
      putStrLn $ "Device Type \t" ++ device' ^. deviceType
      putStrLn $ "Device ID\t" ++ (device' ^. deviceId) ++ "\n"

play :: IO ()
play = do
  accessToken <- requestAccessToken
  status <- API.play accessToken
  if
      | statusIsSuccessful status -> putStrLn "▶ Started Playback" >> exitSuccess
      | status == status403 -> putStrLn "Playback cannot be started, the song may already be playing" >> exitSuccess
      | status == status404 -> die $ "No active devices found"
      | otherwise -> exitUnknownResponse $ status

playTrack :: Uri -> IO ()
playTrack uri' = do
  accessToken <- requestAccessToken
  status <- API.playTrack accessToken uri'
  if
      | statusIsSuccessful status -> putStrLn "▶ Playing Song" >> exitSuccess
      | otherwise -> exitUnknownResponse status

playOnDevice :: DeviceId -> IO ()
playOnDevice deviceId' = do
  accessToken <- requestAccessToken
  status <- API.setPlayer accessToken deviceId'
  if
      | statusIsSuccessful status -> putStrLn "📾  Set Device" >> exitSuccess
      | otherwise -> exitUnknownResponse status

pause :: IO ()
pause = do
  accessToken <- requestAccessToken
  status <- API.pause accessToken
  if
      | statusIsSuccessful status -> putStrLn "⏸ Paused Playback" >> exitSuccess
      | status == status403 -> putStrLn "Playback cannot be paused, the song may already be paused" >> exitSuccess
      | status == status404 -> die $ "No active devices found"
      | otherwise -> exitUnknownResponse status

next :: IO ()
next = do
  accessToken <- requestAccessToken
  status <- API.next accessToken
  case (status ^. code) of
    204 -> putStrLn "⏭️ Skipped to next track" >> exitSuccess
    404 -> die $ "No active devices found"
    _ -> exitUnknownResponse status

previous :: IO ()
previous = do
  accessToken <- requestAccessToken
  status <- API.previous accessToken
  case (status ^. code) of
    204 -> putStrLn "⏮️ Skipped to previous track" >> exitSuccess
    404 -> die $ "No active devices found"
    _ -> exitUnknownResponse status

getVolume :: IO ()
getVolume = do
  accessToken <- requestAccessToken
  (status, response) <- API.getPlayer accessToken
  case (status ^. code, response) of
    (200, Just response') -> printVolume response'
    (204, _) -> die $ "No active devices found"
    _ -> exitUnknownResponse status
  where
    printVolume :: PlayerResponse -> IO ()
    printVolume response = do
      let value = response ^. device ^. volumePercent
      putStrLn ("🔊 Current volume is " ++ show value ++ "%") >> exitSuccess

setVolume :: Int -> IO ()
setVolume value = do
  accessToken <- requestAccessToken
  status <- API.setVolume accessToken value
  if
      | statusIsSuccessful status -> putStrLn ("🎚️ Set volume to " ++ (show value) ++ "%") >> exitSuccess
      | status == status400 -> die $ "Illegal value"
      | status == status404 -> die $ "No active devices found"
      | otherwise -> exitUnknownResponse status

exitUnknownResponse :: Status -> IO ()
exitUnknownResponse status = die $ "An unknown error occured (http status code:" ++ show (status ^. code) ++ ")"
