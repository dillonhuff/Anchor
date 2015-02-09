module RosenbergPulse() where

rosenbergPulse :: Int -> Int -> Double -> Double -> [Double]
rosenbergPulse f0 fs dutyCycle glottalOpenFrac = [0]

pulsePhases :: Int ->
               Int ->
               Int ->
               [Double]
pulsePhases n1 n2 tailLength =
  let startCycle = rosenbergStart n1
      midCycle = rosenbergMid n1 n2
      end = replicate tailLength 0 in
  startCycle ++ midCycle ++ end

rosenbergStart :: Int -> [Double]
rosenbergStart n1 = map rosenbergStartFunc $ map fromIntegral [1..(n1 - 1)]
  where
    rosenbergStartFunc n = 0.5*(1.0 - cos(pi*(n-1)/(fromIntegral n1)))

rosenbergMid :: Int -> Int -> [Double]
rosenbergMid n1 n2 = map rosenbergMidFunc $ map fromIntegral [n1..n2]
  where
    rosenbergMidFunc n = cos $ pi*(n - (fromIntegral n1))/(2*(fromIntegral n2))
