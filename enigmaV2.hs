module Enigma (module Enigma) where

import Data.Char
import Data.List
import Data.Map as Map
import Data.Maybe

--Alphabet
alphabet :: String
alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

indexToChar :: Int -> Char
indexToChar n
    | n `elem` [0..25] = alphabet !! n

charToIndex :: Char -> Int
charToIndex c = fromJust(elemIndex c alphabet)

--Remove whitespaces from strings
removeWhiteAux :: String -> String -> String
removeWhiteAux "" strng = strng
removeWhiteAux (c:cs) strng
    | c == ' '  = removeWhiteAux cs strng
    | otherwise = removeWhiteAux cs (c:strng)

removeWhite :: String -> String
removeWhite cs = reverse.removeWhiteAux cs $ ""

rotateLevAux :: [Int] -> [[Int]]
rotateLevAux xs = take len $ Data.List.map (take len) $ tails $ cycle xs
    where
        len = length xs

rotateLev :: Int ->  [Int] -> [Int]
rotateLev n xs = (rotateLevAux xs) !! n

--Rotor [notches] [cover] ringStellungDisplacement [forwardTransformFunction] [backwardTransformFunction]
data Rotor = Rotor [Int] [Int] Int [Int] [Int] deriving Show
data RotorSet = RotorSet Rotor Rotor Rotor Rotor Rotor deriving Show
type Plugboard = Map.Map Int Int

generateRotor :: Int -> Int -> Int -> Rotor
generateRotor rotorVersion ringStellung grundStellung
    | rotorVersion == 1  =  Rotor [16]     (rotateLev ((grundStellung-ringStellung) `mod` 25) (rotateLev (ringStellung `mod` 25) [0..25])) ringStellung (rotateLev ((grundStellung-ringStellung) `mod` 25) [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9]) (rotateLev ((grundStellung-ringStellung) `mod` 25) [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9])
    | rotorVersion == 2  =  Rotor [4]      (rotateLev ((grundStellung-ringStellung) `mod` 25) (rotateLev (ringStellung `mod` 25) [0..25])) ringStellung (rotateLev ((grundStellung-ringStellung) `mod` 25) [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4]) (rotateLev ((grundStellung-ringStellung) `mod` 25) [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18])
    | rotorVersion == 3  =  Rotor [21]     (rotateLev ((grundStellung-ringStellung) `mod` 25) (rotateLev (ringStellung `mod` 25) [0..25])) ringStellung (rotateLev ((grundStellung-ringStellung) `mod` 25) [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14]) (rotateLev ((grundStellung-ringStellung) `mod` 25) [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12])
    | rotorVersion == 4  =  Rotor [9]      (rotateLev ((grundStellung-ringStellung) `mod` 25) (rotateLev (ringStellung `mod` 25) [0..25])) ringStellung (rotateLev ((grundStellung-ringStellung) `mod` 25) [4,18,14,21,15,25,9,0,24,16,20,8,17,7,23,11,13,5,19,6,10,3,2,12,22,1]) (rotateLev ((grundStellung-ringStellung) `mod` 25) [7,25,22,21,0,17,19,13,11,6,20,15,23,16,2,4,9,12,1,18,10,3,24,14,8,5])
    | rotorVersion == 5  =  Rotor [25]     (rotateLev ((grundStellung-ringStellung) `mod` 25) (rotateLev (ringStellung `mod` 25) [0..25])) ringStellung (rotateLev ((grundStellung-ringStellung) `mod` 25) [21,25,1,17,6,8,19,24,20,15,18,3,13,7,11,23,0,22,12,9,16,14,5,4,2,10]) (rotateLev ((grundStellung-ringStellung) `mod` 25) [16,2,24,11,23,22,4,13,5,19,25,14,18,12,21,9,20,3,10,6,8,0,17,15,7,1])
    | rotorVersion == 6  =  Rotor [25,12]  (rotateLev ((grundStellung-ringStellung) `mod` 25) (rotateLev (ringStellung `mod` 25) [0..25])) ringStellung (rotateLev ((grundStellung-ringStellung) `mod` 25) [9,15,6,21,14,20,12,5,24,16,1,4,13,7,25,17,3,10,0,18,23,11,8,2,19,22]) (rotateLev ((grundStellung-ringStellung) `mod` 25) [18,10,23,16,11,7,2,13,22,0,17,21,6,12,4,1,9,15,19,24,5,3,25,20,8,14])
    | rotorVersion == 7  =  Rotor [25,12]  (rotateLev ((grundStellung-ringStellung) `mod` 25) (rotateLev (ringStellung `mod` 25) [0..25])) ringStellung (rotateLev ((grundStellung-ringStellung) `mod` 25) [13,25,9,7,6,17,2,23,12,24,18,22,1,14,20,5,0,8,21,11,15,4,10,16,3,19]) (rotateLev ((grundStellung-ringStellung) `mod` 25) [16,12,6,24,21,15,4,3,17,2,22,19,8,0,13,20,23,5,10,25,14,18,11,7,9,1])
    | rotorVersion == 8  =  Rotor [25,12]  (rotateLev ((grundStellung-ringStellung) `mod` 25) (rotateLev (ringStellung `mod` 25) [0..25])) ringStellung (rotateLev ((grundStellung-ringStellung) `mod` 25) [5,10,16,7,19,11,23,14,2,1,9,18,15,3,25,17,0,12,4,22,13,8,20,24,6,21]) (rotateLev ((grundStellung-ringStellung) `mod` 25) [16,9,8,13,18,0,24,3,21,10,1,5,17,20,7,12,2,15,11,4,22,25,19,6,23,14])
    | rotorVersion == 9  =  Rotor []       (rotateLev ((grundStellung-ringStellung) `mod` 25) (rotateLev (ringStellung `mod` 25) [0..25])) ringStellung (rotateLev ((grundStellung-ringStellung) `mod` 25) [11,4,24,9,21,2,13,8,23,22,15,1,16,12,3,17,19,0,10,25,6,5,20,7,14,18]) (rotateLev ((grundStellung-ringStellung) `mod` 25) [17,11,5,14,1,21,20,23,7,3,18,0,13,6,24,10,12,15,25,16,22,4,9,8,2,19]) --beta
    | rotorVersion == 10 =  Rotor []       (rotateLev ((grundStellung-ringStellung) `mod` 25) (rotateLev (ringStellung `mod` 25) [0..25])) ringStellung (rotateLev ((grundStellung-ringStellung) `mod` 25) [5,18,14,10,0,13,20,4,17,7,12,1,19,8,24,2,22,11,16,15,25,23,21,6,9,3]) (rotateLev ((grundStellung-ringStellung) `mod` 25) [4,11,15,25,7,0,23,9,13,24,3,17,10,5,2,19,18,8,1,12,6,22,16,21,14,20]) --gamma



generateReflector :: String -> Rotor
generateReflector reflectorVersion
    | reflectorVersion == "B"     = Rotor [] [0..25] 0 [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19] []
    | reflectorVersion == "C"     = Rotor [] [0..25] 0 [5,21,15,9,8,0,14,24,4,3,17,25,23,22,6,2,19,10,20,16,18,1,13,12,7,11] []
    | reflectorVersion == "Bthin" = Rotor [] [0..25] 0 [4,13,10,16,0,20,24,22,9,8,2,14,15,1,11,12,3,23,25,21,5,19,7,17,6,18] []
    | reflectorVersion == "Cthin" = Rotor [] [0..25] 0 [17,3,14,1,9,13,19,10,21,4,7,12,11,5,2,22,25,0,23,6,24,8,15,18,20,16] []

generatePlugboardAux :: Int -> String -> Plugboard
generatePlugboardAux 0 _  = Map.empty
generatePlugboardAux _ strng | length strng < 2 = Map.empty
generatePlugboardAux n (f:s:cs) = Map.insert (charToIndex s) (charToIndex f) (Map.insert (charToIndex f) (charToIndex s) (generatePlugboardAux (n-1) cs))

generatePlugboard :: String -> Plugboard
generatePlugboard strng = generatePlugboardAux 10 cs where
    cs = removeWhite(strng)

getPlugboardConnection :: Int -> Plugboard -> Int
getPlugboardConnection index plugBoard
    | (Map.member index plugBoard) = plugBoard ! index
    | otherwise = index

rotorTransformation :: Int -> RotorSet -> Int
rotorTransformation input (RotorSet reflector rotor4 rotor3 rotor2 rotor1) = Data.List.foldr (\x y -> backwardTransform x y) (Data.List.foldr (\x y -> forwardTransform x y) input [reflector, rotor4, rotor3, rotor2, rotor1]) [rotor1, rotor2, rotor3, rotor4]

nextRotorSet :: RotorSet -> RotorSet
nextRotorSet (RotorSet reflector rotor4 rotor3 rotor2 rotor1)
    | (not i) && (not ii) = RotorSet reflector rotor4 rotor3 rotor2 (nextRotation rotor1)
    | (not i) && ii       = RotorSet reflector rotor4 (nextRotation rotor3) (nextRotation rotor2) (nextRotation rotor1)
    | i && (not ii)       = RotorSet reflector rotor4 rotor3 (nextRotation rotor2) (nextRotation rotor1)
    | i && ii             = RotorSet reflector rotor4 (nextRotation rotor3) (nextRotation rotor2) (nextRotation rotor1)
        where
            i  = readyToRotate(rotor1)
            ii = readyToRotate(rotor2)

class Transformation a where
    forwardTransform :: a -> Int -> Int
    backwardTransform :: a -> Int -> Int
    readyToRotate :: a -> Bool
    nextRotation :: a -> a

instance Transformation Rotor where
    forwardTransform (Rotor _ cover ringStellungDisplacement forwardTransformFunction _) request = ((forwardTransformFunction !! request) - (cover !! 0) + ringStellungDisplacement) `mod` 26
    backwardTransform (Rotor _ cover ringStellungDisplacement _ backwardTransformFunction) request = ((backwardTransformFunction !! request) - (cover !! 0) + ringStellungDisplacement) `mod` 26
    readyToRotate (Rotor notches cover _ _ _) = (cover !! 0) `elem` notches
    nextRotation (Rotor notches cover ringStellungDisplacement forwardTransformFunction backwardTransformFunction) = Rotor notches (rotateLev 1 cover) ringStellungDisplacement (rotateLev 1 forwardTransformFunction) (rotateLev 1 backwardTransformFunction)



enigmaTransformationAux :: RotorSet -> Plugboard -> Char -> Char
enigmaTransformationAux rotorSet plugBoard x = indexToChar (getPlugboardConnection (rotorTransformation (getPlugboardConnection (charToIndex x) plugBoard) rotorSet) plugBoard)

enigmaTransformation :: RotorSet -> Plugboard -> String -> String
enigmaTransformation _ _ [] = ""
enigmaTransformation rotorSet plugBoard (x:xs) = (enigmaTransformationAux (nextRotorSet (nextRotorSet rotorSet)) plugBoard x):(enigmaTransformation (nextRotorSet rotorSet) plugBoard xs)
