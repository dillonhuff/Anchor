module FormantFilter(Formant,
                     formant,
                     applyFormantFilter) where

import Signal

data Formant = Formant {
     frequency :: Double,
     bandwidth :: Double
  } deriving (Eq, Ord, Show)

formant :: Double -> Double -> Formant
formant = Formant

applyFormantFilter :: Formant -> Signal -> Signal
applyFormantFilter form sig =
  let (r, omega) = normedROmega form (samplesPerSecond sig)
      filteredSamps = recFormantFilter r omega 0.0 0.0 (samples sig) in
  signal (samplesPerSecond sig) filteredSamps

yt :: Double -> Double -> Double -> Double -> Double -> Double
yt r omega ut yt1 yt2 =
  let b = (-1)*2*r*cos(omega)
      k = (-1)*r*r
      a = 1 - 2*r*cos(omega) + r*r in
  a*ut + b*yt1 + k*yt2

normedROmega :: Formant -> Int -> (Double, Double)
normedROmega form sampleRate =
  let b = (bandwidth form)*2*pi / (fromIntegral sampleRate)
      f = (frequency form)*2*pi / (fromIntegral sampleRate)
      r = exp $ (-1)*b/2 in
  (r, f)

recFormantFilter :: Double -> Double -> Double -> Double -> [Double] -> [Double]
recFormantFilter _ _ _ _ [] = []
recFormantFilter r omega yt1 yt2 samples =
  let ut = head samples
      nextYt1 = (yt r omega ut yt1 yt2) in
  nextYt1 : (recFormantFilter r omega nextYt1 yt1 (tail samples))
