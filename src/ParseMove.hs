module ParseMove where

import Data.Char (toLower, ord)
import Text.Read (readMaybe)
import Move
import Piece

validX :: Char -> Bool
validX c = ('a' <= c && c <= 'k') || ('A' <= c && c <= 'K')

validY :: String -> Bool
validY [c] = '0' <= c && c <= '9'
validY [c1,c2] = [c1,c2] == "10"
validY _ = False

createPos :: Int -> Maybe Int -> Maybe Pos
createPos x (Just y) = Just (x,y)
createPos _ _ = Nothing

parsePos :: String -> Maybe Pos
parsePos (c1:c2) =
    if validX c1 && validY c2 then
        createPos x y
    else
        Nothing
    where
        x = ord (toLower c1) - ord 'a'
        y = readMaybe c2 :: Maybe Int
parsePos _ = Nothing

createMove :: Maybe Pos -> Maybe Pos -> Maybe Move
createMove (Just p1) (Just p2) = Just (p1, p2)
createMove _ _ = Nothing

parsePositions :: [String] -> Maybe Move
parsePositions [s1,s2] =
    createMove (parsePos s1) (parsePos s2)
parsePositions _ = Nothing

parseMove :: String -> Maybe Move
parseMove line =
        parsePositions $ words line
