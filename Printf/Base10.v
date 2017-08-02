Require Import NAxioms NProperties OrdersFacts.
Require Import Nat.
Require Import Ascii.
Require Import Program.Wf.
Require Import Omega.
Require Import Arith.PeanoNat.
Require Import String.

Theorem big_step_induction_helper : forall s:nat, s > 0 -> forall P:nat->Prop, 
  (forall n, n < s -> P n) -> 
  (forall n, P n -> P (n + s)) -> forall n, forall s', s' <= s -> P (n + s').
induction n.
firstorder.
inversion H2.
apply H1.
auto.
simpl.
apply H0.
omega.
intros.
inversion H2.
apply H1.
assert (S n = n + 1).
omega.
rewrite H4.
apply IHn.
omega.
assert (S n + s' = n + S s').
omega.
rewrite H5.
apply IHn.
omega.
Qed.

Theorem big_step_induction : forall s:nat, s > 0 -> forall P:nat->Prop, 
  (forall n, n < s -> P n) -> 
  (forall n, P n -> P (n + s)) -> forall n, P n.
intros.
assert (n = n + 0).
auto.
rewrite H2.
apply big_step_induction_helper with s; auto.
omega.
Qed.

Program Fixpoint last_digit_to_char (n:nat) {measure n} : ascii :=
match n with
| 0 => "0"
| 1 => "1"
| 2 => "2"
| 3 => "3"
| 4 => "4"
| 5 => "5"
| 6 => "6"
| 7 => "7"
| 8 => "8"
| 9 => "9"
| n' => last_digit_to_char (n' - 10)
end.

Obligation 1.
destruct n; firstorder.
Defined.
Obligation 2.
firstorder.
Defined.

Fixpoint div10 (n:nat) : nat :=
match n with
| 0|1|2|3|4|5|6|7|8|9 => 0
| S(S(S(S(S(S(S(S(S(S(n')))))))))) => S (div10 n')
end.

Definition S10 (n:nat) := S(S(S(S(S(S(S(S(S(S(n)))))))))).

Theorem div10_dec : forall n:nat, n >= 10 -> div10 n < n.
intros.
assert (forall n, div10 (10 + n) < (10 + n)).
apply big_step_induction with 10; try omega; try firstorder.
repeat(destruct n0; [cbv; auto with arith|try omega]).
simpl.
rewrite Nat.add_comm.
omega.
assert (exists n', n = 10 + n').
repeat(destruct n; [omega|]).
exists n.
auto.
destruct H1.
rewrite H1.
intuition.
Qed.


Program Fixpoint nat_to_str (n:nat) {measure n} : string :=
match n as n1 with
| 0|1|2|3|4|5|6|7|8|9 => String (last_digit_to_char n) EmptyString
| n' => nat_to_str (div10 n') ++ String (last_digit_to_char n) EmptyString
end.
Obligation 1.
apply div10_dec.
omega.
Qed.
Obligation 2.
intuition.
Qed.

Require Import ZArith.

Definition z_to_str (z:Z) : string :=
match z with
| Z0 => "0"
| Zpos x => nat_to_str (Pos.to_nat x)
| Zneg x => String "-" (nat_to_str (Pos.to_nat x))
end.

