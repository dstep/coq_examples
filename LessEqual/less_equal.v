(* Definition for LessEqual *)
Inductive LE 
	(* It is a property of two natural numbers *)
	: nat -> nat -> Prop :=
(* zero is less thatn or equal to any natural number *)
	| LEO : forall n:nat, LE 0 n
(* if n <= m, so Succ(n) <= Succ(m) *)
	| LES : forall n m:nat, LE n m -> LE (S n) (S m).

(* Definition for Or *)	
Inductive Or (A B : Prop) : Set :=
| Left : A -> Or A B 
| Right : B -> Or A B.


(* decision procedure for LE, taking two natural numbers... *)
Fixpoint dec (n m:nat)
	(* recursing on the first argument (required for termination checking)... *)
	{struct n} 
	(* returning either a proof that n <= m, or a proof that m <= n *)
	: Or (LE n m) (LE m n) :=
	(* pattern matching on n,m *)
	match n, m 
		(* return type must be explicit, because different
		  branches have different types *)
		return Or (LE n m) (LE m n) with
	(* for 0,x, it returns an object (LEO x) which is of type
		LE 0 x, so matches the left case, LE n m *)
	| O, x => Left (LE 0 x) (LE x 0) (LEO x)
	(* for x,0, it returns an object (LEO x) which is of type
		LE 0 x, so matches the right case, LE m n *)
	| x, O => Right (LE x 0) (LE 0 x) (LEO x)
	(* for Succ(n), Succ(m), it recurses, and builds a proof from the result of that recursion *)
	| S n as n', S m as m' =>
		match dec n m with
		| Left _ _ h => Left (LE n' m') (LE m' n') (LES n m h)
		| Right _ _ h => Right (LE n' m') (LE m' n') (LES m n h)
		end
	end.

Extraction Language Ocaml.
Recursive Extraction dec.

(*
type nat =
| O
| S of nat

(** In OCaml, proofs collapse to a boolean type **)
type or0 =
| Left
| Right

(** So decision procedure collapses to a simple is_less_then **)
(** val dec : nat -> nat -> or0 **)
  let rec dec n m =
    match n with
    | O -> Left
    | S n0 ->
      (match m with
       | O -> Right
       | S m0 -> dec n0 m0)
*)