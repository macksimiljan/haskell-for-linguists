-- | > meineLaenge "hallo welt!"
-- | 11
meineLaenge :: String -> Int
meineLaenge []           = 0
meineLaenge (first:tail) = 1 + meineLaenge tail
