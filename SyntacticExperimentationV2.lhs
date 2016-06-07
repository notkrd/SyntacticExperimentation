

\begin{code}

module SyntacticExperimentationV2 where

import Data.Set (Set)
import qualified Data.Set as Set

data WordCat =
    NA
  | PrimativeCat (Set String)
  | RSlashCat {r_targ_cat :: WordCat, r_arg_cat :: WordCat, r_kinds :: Set String}
  | LSlashCat {l_targ_cat :: WordCat, l_arg_cat :: WordCat, l_kinds :: Set String}
    deriving (Eq, Ord)

(@/@) :: WordCat -> WordCat -> WordCat
(@/@) cat_a cat_b = RSlashCat cat_a cat_b Set.empty

(@\@) :: WordCat -> WordCat -> WordCat
(@\@) cat_a cat_b = LSlashCat cat_a cat_b Set.empty

s, n, np, pp :: WordCat
s = PrimativeCat (Set.singleton "S"); n = PrimativeCat (Set.singleton "N");
np = PrimativeCat (Set.singleton "NP"); pp = PrimativeCat (Set.singleton "PP"); 

labels :: WordCat -> Set String
labels (PrimativeCat lbls) = lbls
labels (RSlashCat _ _ lbls) = lbls
labels (LSlashCat _ _ lbls) = lbls

(#%) :: WordCat -> WordCat -> Bool -- True if the first category contains the second
(#%) (PrimativeCat c) (PrimativeCat d) = Set.isSubsetOf c d
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

type Phrase = (String, WordCat)

(@+@) :: Phrase -> Phrase -> Phrase
(@+@) (word_a, cat_a) (word_b, cat_b) = (word_a ++ " " ++ word_b, cat_a #+# cat_b)

newPhrases :: [Phrase] -> [Phrase]
newPhrases all_phrases =
  filter (\phr -> (snd phr) /= NA && notElem phr all_phrases)
         [phr1 @+@ phr2 | phr1 <- all_phrases, phr2 <- all_phrases]

gensOfPhrases :: Integer -> [Phrase] -> [Phrase]
gensOfPhrases 0 all_phrases = all_phrases
gensOfPhrases n all_phrases = gensOfPhrases (n - 1) (all_phrases ++ (newPhrases all_phrases))

makeSentencesGens :: Integer -> [Phrase] -> [String]
makeSentencesGens n all_phrases = map fst (filter (\phr -> Set.member "S" (labels (snd phr)))
                                           (gensOfPhrases n all_phrases))

some_words :: [Phrase]
some_words = [("Jane",np),
              ("road", n),
              ("map", n),
              ("a", np @/@ n),
              ("the", np @/@ n),
              ("is", s @\@ np),
              ("inscribes", (s @\@ np) @/@ np),
              ("map to", n @/@ np)]

other_words :: [Phrase]
other_words = [("the", np @/@ n),
               ("some", np @/@ n),
               ("CEO", n),
               ("bird", n),
               ("and elsewhere", (s @/@ s) @\@ s),
               ("breathes", s @\@ np),
               ("flies", s @\@ np)]

\end{code}
