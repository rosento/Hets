module PLpatt.Sign where

import PLpatt.AS_BASIC_PLpatt
--import Common.Id

data Sigs = Sigs [Decl]
data Theo = Theo{sign :: Sigs,axioms :: [Form]} 