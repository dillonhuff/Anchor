module Signal(Signal,
              samplesPerSecond,
              samples) where

data Signal = Signal {
     samplesPerSecond :: Int,
     samples :: [Double]
  } deriving (Eq, Ord, Show)


sinWave :: Double  -- | Frequency
      -> Int -- | Samples per second
      -> Double -- | Length of sinWave in seconds
      -> Double -- | Volume
      -> [Double]
sinWave freq samples len volume = take (round (len * (fromIntegral samples))) $ 
                         map (* volume) $
                         map sin [0.0, (freq * 2 * pi / (fromIntegral samples))..]
