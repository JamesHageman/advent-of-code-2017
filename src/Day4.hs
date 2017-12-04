module Day4 where

import qualified Data.Set as Set
import Data.List
import Data.Maybe (isJust)

data Passphrase = Passphrase [String] (Set.Set String) deriving (Show)

validate1 :: Passphrase -> Bool
validate1 (Passphrase list set) = Set.size set == length list

validate2 :: Passphrase -> Bool
validate2 (Passphrase list set) = Set.size set2 == length list
  where set2 = Set.foldr (Set.insert . sort) Set.empty set

parsePassphrase :: (Passphrase -> Bool) -> String -> Maybe Passphrase
parsePassphrase validator line = 
  if validator passphrase then
    Just passphrase
    else Nothing
  where
    passwords = words line
    setOfPasswords = Set.fromList passwords
    passphrase = Passphrase passwords setOfPasswords

parsePassphrases :: (Passphrase -> Bool) -> String -> [Maybe Passphrase]
parsePassphrases validator = fmap (parsePassphrase validator) . lines

countValid :: [Maybe a] -> Int
countValid = length . filter isJust

day4part1 = countValid . parsePassphrases validate1 <$> readFile "test/Day4.input"
day4part2 = countValid . parsePassphrases validate2 <$> readFile "test/Day4.input"
