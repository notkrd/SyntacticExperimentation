

\begin{code}

module SyntacticExperimentationV2 where

import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

data WordCat =
    NA
  | PrimitiveCat (Set String)
  | RSlashCat {r_targ_cat :: WordCat, r_arg_cat :: WordCat, r_kinds :: Set String}
  | LSlashCat {l_targ_cat :: WordCat, l_arg_cat :: WordCat, l_kinds :: Set String}
    deriving (Eq, Ord)

(@/@) :: WordCat -> WordCat -> WordCat
(@/@) cat_a cat_b = RSlashCat cat_a cat_b Set.empty

(@\@) :: WordCat -> WordCat -> WordCat
(@\@) cat_a cat_b = LSlashCat cat_a cat_b Set.empty

addLabels :: Set String -> WordCat -> WordCat
addLabels _ NA = NA
addLabels more_labels (PrimitiveCat some_labels) =
  PrimitiveCat (some_labels `Set.union` more_labels)
addLabels more_labels (RSlashCat a b some_labels) =
  RSlashCat a b (some_labels `Set.union` more_labels)
addLabels more_labels (LSlashCat a b some_labels) =
  LSlashCat a b (some_labels `Set.union` more_labels)

addString :: String -> WordCat -> WordCat
addString _ NA = NA
addString new_label (PrimitiveCat some_labels) =
  PrimitiveCat (new_label `Set.insert` some_labels)
addString new_label (RSlashCat a b some_labels) =
  RSlashCat a b (new_label `Set.insert` some_labels)
addString new_label (LSlashCat a b some_labels) =
  LSlashCat a b (new_label `Set.insert` some_labels)

s, n, np, pp :: WordCat
s = PrimitiveCat (Set.singleton "S"); n = PrimitiveCat (Set.singleton "N");
np = PrimitiveCat (Set.singleton "NP"); pp = PrimitiveCat (Set.singleton "PP"); 

labels :: WordCat -> Set String
labels (PrimitiveCat lbls) = lbls
labels (RSlashCat _ _ lbls) = lbls
labels (LSlashCat _ _ lbls) = lbls

(#%) :: WordCat -> WordCat -> Bool -- True if the first category contains the second
(#%) (PrimitiveCat c) (PrimitiveCat d) = Set.isSubsetOf c d
(#%) (RSlashCat targ_a arg_a kinds_a) (RSlashCat targ_b arg_b kinds_b)
  | Set.isSubsetOf kinds_a kinds_b && arg_a #% arg_b && targ_a #% targ_b = True
  | otherwise = False
(#%) (LSlashCat targ_a arg_a kinds_a) (LSlashCat targ_b arg_b kinds_b)
  | Set.isSubsetOf kinds_a kinds_b && arg_a #% arg_b && targ_a #% targ_b = True
  | otherwise = False
(#%) _ _ = False

(#+#) :: WordCat -> WordCat -> WordCat -- Simplifies the category [A,B], or gives NA
(#+#) (RSlashCat cat_a cat_b kinds_1) (LSlashCat cat_c (RSlashCat cat_d cat_e kinds_3) _)
  | (RSlashCat cat_a cat_b kinds_1) #% (RSlashCat cat_d cat_e kinds_3) = cat_c
  | otherwise = NA
(#+#) (RSlashCat cat_a cat_b _) cat_c
  | cat_b #% cat_c = cat_a
  | otherwise = NA
(#+#) cat_a (LSlashCat cat_b cat_c _)
  | cat_c #% cat_a = cat_b
  | otherwise = NA
(#+#) _ _ = NA

(?+?) :: WordCat -> WordCat -> Bool
(?+?) cat_a cat_b = cat_a #+# cat_b /= NA

type Phrase = ([String], WordCat)

(@+@) :: Phrase -> Phrase -> Phrase
(@+@) (phrase_a, cat_a) (phrase_b, cat_b) = (phrase_a ++ phrase_b, cat_a #+# cat_b)

phrasesOfCat :: Set String -> [Phrase] -> [Phrase]
phrasesOfCat some_kind all_phrases =
  filter (\phr -> Set.isSubsetOf some_kind (labels (snd phr))) all_phrases

newPhrases :: [Phrase] -> [Phrase]
newPhrases all_phrases =
  filter (\phr -> (snd phr) /= NA && notElem phr all_phrases)
         [phr1 @+@ phr2 | phr1 <- all_phrases, phr2 <- all_phrases]

gensOfPhrases :: Integer -> [Phrase] -> [Phrase]
gensOfPhrases 0 all_phrases = all_phrases
gensOfPhrases n all_phrases = gensOfPhrases (n - 1) (all_phrases ++ (newPhrases all_phrases))

makeSentencesGens :: Integer -> [Phrase] -> [String]
makeSentencesGens n all_phrases = map (concat . (intersperse " ") . fst) (phrasesOfCat (Set.singleton "S")
                                           (gensOfPhrases n all_phrases))

some_words :: [Phrase]
some_words = [(["Jane"],np),
              (["road"], n),
              (["map"], n),
              (["a"], np @/@ n),
              (["the"], np @/@ n),
              (["is"], s @\@ np),
              (["inscribes"], (s @\@ np) @/@ np),
              (["map","to"], n @/@ np)]

other_words :: [Phrase]
other_words = [(["the"], np @/@ n),
               (["some"], np @/@ n),
               (["CEO"], n),
               (["bird"], n),
               (["and elsewhere"], (s @/@ s) @\@ s),
               (["breathes"], s @\@ np),
               (["flies"], s @\@ np)]

more_words :: [Phrase]
more_words = [(["mouth"], n),
              (["machine"], n),
              (["meanings"], np),
              (["this"], np @/@ n),
              (["that"], np @/@ n),
              (["watches"], (s @\@ np) @/@ np),
              (["follows"], ((s @\@ np) @/@ pp) @/@ np),
              (["into"], pp @/@ np)]

place, agent :: WordCat
place = addString "place" n; agent = addString "agent" np;
just_place = addString "just place" place

city_words :: [Phrase]
city_words = [(["city"], (addString "vague" just_place)),
              (["bakeries"], addString "occupants" np),
              (["pet","cats"], addString "occupants" np),
              (["living"], just_place @/@ (addString "vague" just_place)),
              (["dead"], just_place @/@ (addString  "vague" just_place)),
              (["the"], (addString "place" np) @/@ n),
              (["walks"], ((addString "simple" (addString "travel" s)) @\@ agent)
               @/@ (addString "destination" pp)),
              (["surrounds"], ((addString "simple" (addString "description" s))
                               @\@ (addString "place" np))
               @/@ agent),
              (["mechanically"], (s @\@ agent)
               @/@ ((addString "simple" s) @\@ agent)),
              (["and"], (s @\@ (addString "travel" s)) @/@ (addString "description" s)),
              (["with"], (place @\@ just_place) @/@ (addString "possession" pp)),
              (["its"], (addString "possession" pp) @/@ (addString "occupants" np)),
              (["into"], (addString "destination" pp) @/@ (addString "place" np)),
              (["through"], (addString "destination" pp) @/@ (addString "place" np)),
              (["Quinn"], agent)]

game_words :: [Phrase]
game_words = []

\end{code}
