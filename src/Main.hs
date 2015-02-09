module Main(main) where

import FormantFilter
import Signal
import VowelFormants
import WriteWav

main :: IO ()
main =
  let impulseTrain = concat $ replicate 100 [100, 0]
      impSig = signal 8192 impulseTrain
      filtSig = sequentialFormantFilter aFormants impSig in
  writeOneChannelWAVEWithDateTime "AltAFilter" filtSig
