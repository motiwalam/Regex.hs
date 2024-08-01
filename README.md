# Regex.hs
A regular expression matcher that does zero backtracking, implemented in Haskell.

## Algorithm
The algorithm works by defining two functions:

 1. `matchesEmpty :: RegEx -> Bool` which checks if a regular expression matches the empty string.
 2. `derive :: Char -> RegEx -> RegEx` which, given a character `a` and a regular expression `re`, returns a regular expression `re'` such that a string `a:s` matches `re` if and only if `s` matches `re'`.

Together, these functions enable a simple definition of `matches :: [Char] -> RegEx -> Bool`.

## Credits
I was introduced to this approach of regular expression in Volume 1 of [Software Foundations](https://softwarefoundations.cis.upenn.edu/).

Professor Alexandra Silva [discussed this idea](https://www.cs.uoregon.edu/research/summerschool/summer24/topics.php#Silva) in OPLSS 2024.
