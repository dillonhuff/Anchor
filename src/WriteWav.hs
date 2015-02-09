module WriteWav() where

import Data.Int
import Data.List.Utils
import Data.Time
import Data.WAVE
import System.IO

import Signal

writeWAVE :: String -> WAVE -> IO ()
writeWAVE fileName wav = putWAVEFile fileName wav

dateTimeString :: IO String
dateTimeString = do
  c <- getZonedTime
  return $ replace ":" "-" $ replace " " "_" $ show c

writeWAVEWithDateTime :: String -> WAVE -> IO ()
writeWAVEWithDateTime name wav = do
  dt <- dateTimeString
  let fileName = name ++ dt ++ ".wav" in
    writeWAVE fileName wav

int32ToWAVESamples :: [Int32] -> WAVESamples
int32ToWAVESamples ints = map (:[]) ints

oneChannelWAVEHeader :: Signal -> WAVEHeader
oneChannelWAVEHeader signal = WAVEHeader 1 (samplesPerSecond signal) 32 Nothing

oneChannelWAVE :: Signal -> WAVE
oneChannelWAVE signal =
  let header = oneChannelWAVEHeader signal
      wavSamples = int32ToWAVESamples $ map doubleToSample (samples signal) in
  WAVE header wavSamples
