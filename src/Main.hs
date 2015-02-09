module Main(main) where

import FormantFilter
import Signal
import VowelFormants
import WriteWav

main :: IO ()
main =
  let impulseTrain = concat $ replicate 100 [10, 0, -10]
      impSig = signal 4096 impulseTrain
      filtSigA = sequentialFormantFilter aFormants impSig
      filtSigU = sequentialFormantFilter uFormants impSig in
  do
    writeOneChannelWAVEWithDateTime "ZAFilter" filtSigA
    writeOneChannelWAVEWithDateTime "ZUFilter" filtSigU
