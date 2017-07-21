Set Implicit Arguments.

Inductive vec (A:Set) : nat->Set :=
| vec_ex : forall n:nat, vec A n -> A -> vec A (S n)
| vec_empty : vec A 0.

Fixpoint make_vec (A:Set) (sz:nat) (filler:A) : vec A sz := 
match sz with
| 0 => vec_empty A
| S x => vec_ex (make_vec x filler) filler
end.

Require Coq.Program.Tactics.
Require Import Arith.
Require Import Omega.

Fixpoint vec_get (A:Set) (n:nat) (v:vec A n) (idx:nat) (prop:idx < n) : A.
induction idx.
inversion v.
exact H0.
rewrite H in prop.
omega.
apply IHidx.
omega.
Defined.

Fixpoint vec_set (A:Set) (n:nat) (v:vec A n) (idx:nat) (a:A) (prop:idx < n) : vec A n.
induction idx.
inversion v.
refine (vec_ex H a).
rewrite H; auto.
inversion v.
remember (vec_set A n0 H idx a) as v'_f.
assert (vec A n0).
apply v'_f.
omega.
refine (vec_ex H2 H0).
exact (vec_empty A).
Defined.

CoInductive Stream :=
| stream_next : nat -> Stream -> Stream.

Require Import List.

Notation "% x, P %" := (sigS (fun x => P)) : type_scope.

Definition pull_next (input:Stream) : (nat * Stream) :=
  match input with
  | stream_next n stream' => (n, stream')
  end.

Fixpoint for_set (i:nat) (n:nat) (v:vec nat n) (input:Stream) (proof:i < n) {struct i} : vec nat n.
remember (pull_next input) as nxt.
destruct nxt as [p input'].
remember (vec_set v p proof) as v'.
destruct i.
  (*i == 0*)
  exact v'.  
  (*i != 0*)
  refine (for_set i n v' input' _).
  omega.
Defined.

Definition pred (n:nat) :=
match n with
| O => O
| S n' => n'
end.


Definition my_main (input:Stream) : { n : nat & vec nat n }.
remember (pull_next input) as nxt.
  destruct nxt as [n input'].
  remember (make_vec n 0) as v.

  destruct n.
    (*n == 0*)
    apply existT with 0.
    auto.
    (*n is not zero*)
    refine (existT _ (S n) (for_set v input' _)).
    assert (n < S n).
    auto.
    exact H.
Defined.


Extraction Language Haskell.
Extraction "DynVec.hs" my_main.

