module Regex where

data RegEx =
    -- matches no string
      None
    -- matches empty string
    | Empty
    -- matches single character
    | Single Char
    -- matches sequence of regexes
    | App RegEx RegEx
    -- matches one of two regex
    | Union RegEx RegEx
    -- matches zero or more repetititons of the regex
    | Star RegEx

-- matchesEmpty re returns True if and only if
-- re matches the empty string
matchesEmpty :: RegEx -> Bool
matchesEmpty re = case re of
    -- None matches no strings, not even the empty string
    None        -> False
    -- Empty obviously matches the empty string
    Empty       -> True
    -- Single must match a single character, so it can't match
    -- the empty string
    Single _    -> False
    -- For App r1 r2 to match the empty string, both r1 and r2
    -- must match the empty string
    App r1 r2   -> matchesEmpty r1 && matchesEmpty r2
    -- For Union r1 r2 to match the empty string, one of r1, r2
    -- must match the empty string
    Union r1 r2 -> matchesEmpty r1 || matchesEmpty r2
    -- Star r always matches the empty string, since it matches
    -- *zero* or more occurrences of r
    Star _      -> True

-- derive a re returns a regular expresson re'
-- satisfying the relation: for all strings s,
-- (a:s) matches re if and only if s matches re'
derive :: Char -> RegEx -> RegEx
derive a re = case re of
    -- it is impossible for any non-empty string to
    -- match None or Empty, so there is no derivation
    -- for these cases and hence we return None.
    None        -> None
    Empty       -> None
    -- (a:s) can match Single c if and only if a == c and s == [].
    Single c    -> if a == c then Empty else None
    -- If r1 does not match any empty string, then (a:s)
    -- matches App r1 r2 if and only if s = s0 ++ s1 where
    -- (a:s0) matches r1 and s1 matches r2.
    -- In this case, s must match App (derive a r1) r2.
    -- Now, if r1 does match the empty string, the above case
    -- is not entirely ruled out (since r1 could still match nonempty
    -- strings), but it does make it possible for (a:s) to match 
    -- App r1 r2 but not have any s0, s1 such s = s0 ++ s1 and (a:s0)
    -- matches r1. Thus, in this case, we allow s to match derive a r2.
    App r1 r2   -> let basic = App (derive a r1) r2 in 
                   if not $ matchesEmpty r1 then basic
                                            else Union basic (derive a r2)
    -- (a:s) matches Union r1 r2 if and only if (a:s) matches either r1, r2
    -- if and only if s matches either (derive a r1) or (derive a r2).
    Union r1 r2 -> Union (derive a r1) (derive a r2)
    -- Since (a:s) is nonempty, the only way (a:s) matches (Star r) is
    -- if (a:s) matches at least one occurrence of r. In other words,
    -- (a:s) matches (Star r) if and only if s = s0 ++ s1 where
    -- (a:s0) matches r and s1 matches Star r, which in turn occurs
    -- if and only if s0 matches derive a r and s1 matches Star r, which
    -- is to say that s matches App (derive a r) (Star r).
    Star r      -> App (derive a r) (Star r)

-- to test if the empty string matches re,
-- simply use matchesEmpty
-- otherwise, by the defining property of derive,
-- (a:s) matches re if and only if s matches (derive a re)
-- so we can recurse.
matches :: [Char] -> RegEx -> Bool
matches [] re = matchesEmpty re
matches (a:s) re = matches s (derive a re)
