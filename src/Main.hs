module Main where

import qualified Data.ByteString.Lazy as BL

import MyCsvParser
import Utilities

main = do
--     -- terminal output
--     putStrLn "Hello World"
--     print 42
--     -- terminal input
--     putStr "Enter a line: "
--     str <- getLine
--     print str
--     putStr "Enter a character: "
--     char <- getChar
--     putStrLn "'print' twice:"
--     print char
--     print char
--     putStrLn "'putChar' twice:"
--     putChar char
--     putChar char
--
--     putStr "\n####################\n"

    contentHeights <- BL.readFile "data/heights.csv"
    let heightValues = justHeightValues $ parseHeights contentHeights
    print $ minimum heightValues

    putStr "\n####################\n\n"

    content0 <- readFile "data/test_sentences_old.csv"
    let testSentences = lines content0
    putStr $ list2Str $ zip testSentences $ sentences2Grammaticality testSentences

    putStr "\n####################\n\n"

    content <- BL.readFile "data/test_sentences.csv"
    let parsedContent = parseTestSentences content
    let vectorContent = either2Vector parsedContent
    print vectorContent
    print $ "#grammatisch (erwartet): " ++ (show $ length $ filter id (expectations parsedContent))

    putStr "\n####################\n\n"

    let resultRecognizer = sentences2Grammaticality $ sentences parsedContent
    let updatedVectorContent = updateActualFields vectorContent resultRecognizer
    print updatedVectorContent
    BL.writeFile "data/test_sentences_recog.csv" $ testSentences2Csv updatedVectorContent




