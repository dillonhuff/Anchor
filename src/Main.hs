module Main(main) where

import FormantFilter
import Signal
import WriteWav

main :: IO ()
main =
  let impulseTrain = concat $ replicate 100 [1, -1]
      impSig = signal 8192 impulseTrain
      filtSig = applyFormantFilter (formant 2200 170) impSig in
  writeOneChannelWAVEWithDateTime "AltPeriodicImpulse" filtSig
