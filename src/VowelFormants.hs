module VowelFormants(aFormants,
                     uFormants) where

import FormantFilter

aFormants = [formant 720 600, formant 1300 500, formant 2200 700]
uFormants = [formant 0 300, formant 1100 500, formant 1950 400]

