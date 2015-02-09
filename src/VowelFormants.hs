module VowelFormants(sequentialFormantFilter,
                     aFormants) where

import FormantFilter
import Signal

sequentialFormantFilter :: [Formant] -> Signal -> Signal
sequentialFormantFilter formants sig = foldr applyFormantFilter sig formants

aFormants = [formant 720 600, formant 1300 500, formant 2200 700]

