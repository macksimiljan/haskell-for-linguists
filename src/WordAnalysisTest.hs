module WordAnalysisTest where

import WordAnalysis

testsSuccessfull = test0 &&
                   test1 &&
                   test2 &&
                   test3

test0 = isGermanNoun "Hans"
test1 = not $ isGermanNoun "lachen"
test2 = numberPlosives "Krieg" == 2
test3 = numberPlosives "Gerne" == 1