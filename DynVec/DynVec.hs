module DynVec where

import qualified Prelude

__ :: any
__ = Prelude.error "Logical or arity value used"

eq_rect :: a1 -> a2 -> a1 -> a2
eq_rect _ f _ =
  f

eq_rec :: a1 -> a2 -> a1 -> a2
eq_rec x f y =
  eq_rect x f y

eq_rec_r :: a1 -> a2 -> a1 -> a2
eq_rec_r x h y =
  eq_rec x h y

data Nat =
   O
 | S Nat

nat_rect :: a1 -> (Nat -> a1 -> a1) -> Nat -> a1
nat_rect f f0 n =
  case n of {
   O -> f;
   S n0 -> f0 n0 (nat_rect f f0 n0)}

nat_rec :: a1 -> (Nat -> a1 -> a1) -> Nat -> a1
nat_rec =
  nat_rect

data Prod a b =
   Pair a b

data SigT a p =
   ExistT a p

data Vec a =
   Vec_ex Nat (Vec a) a
 | Vec_empty

make_vec :: Nat -> a1 -> Vec a1
make_vec sz filler =
  case sz of {
   O -> Vec_empty;
   S x -> Vec_ex x (make_vec x filler) filler}

vec_set :: Nat -> (Vec a1) -> Nat -> a1 -> Vec a1
vec_set n v idx a =
  nat_rec (\_ ->
    case v of {
     Vec_ex n0 h h0 -> eq_rec (S n0) (\h1 _ -> Vec_ex n0 h1 a) n h h0;
     Vec_empty -> eq_rec O (eq_rec_r n v O) n}) (\idx0 _ _ ->
    case v of {
     Vec_ex n0 h h0 ->
      eq_rec (S n0) (\h1 h2 -> let {v'_f = \_ -> vec_set n0 h1 idx0 a} in let {h3 = v'_f __} in Vec_ex n0 h3 h2) n h
        h0;
     Vec_empty -> eq_rec O Vec_empty n}) idx __

data Stream =
   Stream_next Nat Stream

pull_next :: Stream -> Prod Nat Stream
pull_next input =
  case input of {
   Stream_next n stream' -> Pair n stream'}

for_set :: Nat -> Nat -> (Vec Nat) -> Stream -> Vec Nat
for_set i n v input =
  let {nxt = pull_next input} in
  case nxt of {
   Pair p input' ->
    let {v' = vec_set n v i p} in
    case i of {
     O -> v';
     S i0 -> for_set i0 n v' input'}}

my_main :: Stream -> SigT Nat (Vec Nat)
my_main input =
  let {nxt = pull_next input} in
  case nxt of {
   Pair n input' ->
    let {v = make_vec n O} in
    case n of {
     O -> ExistT O v;
     S n0 -> ExistT (S n0) (for_set n0 (S n0) v input')}}

