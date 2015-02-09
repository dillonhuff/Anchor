module Signal(Signal,
              signal,
              samplesPerSecond,
              samples) where

data Signal = Signal {
     samplesPerSecond :: Int,
     samples :: [Double]
  } deriving (Eq, Ord, Show)

signal = Signal

sinWave :: Double ->
           Int ->
           Double ->
           Double ->
           Signal
sinWave freq samplesPerSec len volume =
  let samps = take (round (len * (fromIntegral samplesPerSec))) $ map (* volume) $ map sin [0.0, (freq * 2 * pi / (fromIntegral samplesPerSec))..] in
  Signal samplesPerSec samps
