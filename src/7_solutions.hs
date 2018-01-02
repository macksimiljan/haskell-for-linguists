import LeafyTrees
import ParseTest

hans'' = mkTree "Hans" [] :: Tree String
lacht'' = mkTree "lacht" [] :: Tree String
vp'' = mkTree "VP" [lacht'']
np'' = mkTree "NP" [hans'']
s'' = mkTree "S" [np'', vp'']

hans' = mkTree (Left "Hans") [] :: LeafyTree String String
lacht' = mkTree (Left "lacht") [] :: LeafyTree String String
vp' = mkTree (Right "VP") [lacht']
np' = mkTree (Right "NP") [hans']
s' = mkTree (Right "S") [np', vp']

arthur = name "Arthur"
maria = name "Maria"
sleep = ivp "slept"
like = tvp "liked"
johnSlept = sleep arthur
johnLikedMary = like arthur maria

slept_np = gap_ivp "slept"
thatSlept = rel $ gap_ivp "slept"
mariaThatSlept = noun_modifier thatSlept maria
arthurLikedMariaThatSlept = like arthur mariaThatSlept
