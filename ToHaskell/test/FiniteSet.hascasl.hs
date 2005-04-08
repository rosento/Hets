{-
instances:
(Eq (FiniteSet a1), (derived__Prelude_Eq_FiniteSet, []))
(Ord (FiniteSet a1), (derived__Prelude_Ord_FiniteSet, []))
(Show (FiniteSet a1), (derived__Prelude_Show_FiniteSet, []))

types:
FiniteSet :: (*->*, data)

values:
a___2_M_2 ::
    forall a . (FiniteSet a, FiniteSet a) -> FiniteSet a
a___2_P_2 ::
    forall a . (FiniteSet a, FiniteSet a) -> FiniteSet a
a___b_2_r :: forall a . a -> FiniteSet a
a___b_r :: forall a . FiniteSet a
derived__Prelude_Eq_FiniteSet ::
    forall a . Eq (FiniteSet a)
derived__Prelude_Ord_FiniteSet ::
    forall a . Ord (FiniteSet a)
derived__Prelude_Show_FiniteSet ::
    forall a . Show (FiniteSet a)
FiniteSet :: forall a . FiniteSet a

scope:
Prelude.FiniteSet |-> Prelude.FiniteSet, Type [FiniteSet] []
Prelude.FiniteSet |-> Prelude.FiniteSet, con of FiniteSet
Prelude.a___2_M_2 |-> Prelude.a___2_M_2, Value
Prelude.a___2_P_2 |-> Prelude.a___2_P_2, Value
Prelude.a___b_2_r |-> Prelude.a___b_2_r, Value
Prelude.a___b_r |-> Prelude.a___b_r, Value
FiniteSet |-> Prelude.FiniteSet, Type [FiniteSet] []
FiniteSet |-> Prelude.FiniteSet, con of FiniteSet
a___2_M_2 |-> Prelude.a___2_M_2, Value
a___2_P_2 |-> Prelude.a___2_P_2, Value
a___b_2_r |-> Prelude.a___b_2_r, Value
a___b_r |-> Prelude.a___b_r, Value
-}
module Dummy where
import MyLogic
data FiniteSet a1
    = FiniteSet deriving (Show, Eq, Ord)
a___2_P_2
    :: (FiniteSet a, FiniteSet a) -> FiniteSet a
a___2_P_2
    =   error{-((FiniteSet a, FiniteSet a) ->
                FiniteSet a)-}
            "a___2_P_2"
a___2_M_2
    :: (FiniteSet a, FiniteSet a) -> FiniteSet a
a___2_M_2
    =   error{-((FiniteSet a, FiniteSet a) ->
                FiniteSet a)-}
            "a___2_M_2"
a___b_2_r :: a -> FiniteSet a
a___b_2_r = error{-(a -> FiniteSet a)-} "a___b_2_r"
a___b_r :: FiniteSet a
a___b_r = error{-(FiniteSet a)-} "a___b_r"
