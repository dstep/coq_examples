(* Standard library for strings *)
Require Import String.
Require Import Ascii.

(* So "xyz" notation is a string now *)
Open Scope string.

(* Gets a format string and returns a type of the printf function with that format string *)
Fixpoint TypeForFormatString (s:string) : Type :=
match s with
| EmptyString => string
| String "#" xs => nat-> TypeForFormatString xs
| String "%" xs => string->TypeForFormatString xs
| String _ xs => TypeForFormatString xs
end.

(* Some assertions *)
(* "cbv; auto" means it is checked by a direct evaluation *)
Goal (TypeForFormatString "no format specifiers" = string). 
cbv; auto.
Qed.

Goal (TypeForFormatString "a single string expected: %" = (string->string)). 
cbv; auto.
Qed.

Goal (TypeForFormatString "String and two numbers: % (#, #)" = (string->nat->nat->string)). 
cbv; auto.
Qed.

(* The following is about converting number to a binary string representation *)
Section NatToString.

  (* Extracts the last digit of a number as an ascii character *)
  Fixpoint last_digit (n:nat) : ascii :=
  match n with
  | O => "0"
  | S O => "1"
  | S (S n') => last_digit n'
  end.

  (* Special stuff for non-structural recursion *)
  Require Import Program.Wf.

  (* Division by 2 *)
  Fixpoint div2 (n:nat) : nat :=
  match n with
  | O => O
  | S O => O
  | S (S n') => S (div2 n')
  end.

  (* Useful theorems for proving the next properties *)
  Require Import Arith.
  Require Import Omega.

  (* Every non-zero number divided by two is less than the original number *)
  Theorem div_decreases : forall n:nat, n <> O -> div2 n < n.
  induction n; firstorder.
  destruct n; firstorder.
  simpl.
  apply Lt.lt_n_S.
  cut (div2 n <= div2 (S n)).
  omega.

    Lemma div2_nonincr_helper : forall n:nat, div2 n <= div2 (S n) /\ div2 (S n) <= div2 (S (S n)).
    induction n.
    firstorder.
    destruct IHn.
    constructor.
    auto.
    simpl div2 at 1.
    remember (S n) as Sn.
    simpl.
    omega.
    Qed.

    Lemma div2_nonincr : forall n:nat, div2 n <= div2 (S n).
    apply div2_nonincr_helper.
    Qed.

  apply div2_nonincr.
  Qed.

  (* Finally, the program for converting number to a string *)
  (* measure answers the question "why this recursive function terminating?"
     {measure n} means "because n decreases"  *)
  Program Fixpoint nat_to_str (n:nat) {measure n} : string :=
  match div2 n as n1 with
  | O => String (last_digit n) EmptyString
  | S n' => (nat_to_str (S n') (*the proof that n' < n is Obligation 1*)) ++ String (last_digit n) EmptyString
  end.

  (* Proving that n is atually decreases with every recursive call *)
  Obligation 1.
  apply Lt.lt_S_n.
  rewrite Heq_n1.
  apply Lt.lt_n_S.
  apply div_decreases.
  destruct n.
  inversion Heq_n1.
  auto.
  (* Obligation is solved, so program can now be accepted by Coq *)
  Defined.
End NatToString.

(* The core printf function. Notice how its return type depends on the parameter s *)
Fixpoint Printf_ (s:string) (pref:string) : TypeForFormatString(s) :=
match s as s' return TypeForFormatString(s') with
  (*With a dependent type, you need to state upfront what type match must return*)
| EmptyString => pref
| String "#" xs => fun x => Printf_ xs (pref ++ nat_to_str x)
| String "%" xs => fun s => Printf_ xs (pref ++ s)
| String x xs => Printf_ xs (pref ++ String x EmptyString)
end.

(* Wrapper for printf *)
Definition Printf (s:string) := Printf_ s ""%string.

(* A few tests *)
Eval compute in (Printf "My format string: % % %" "test" "q" "w")%string.
(* = "My format string: test q w" *)
Eval compute in (Printf "Current year is # and I'm # years old" (2017) (2017-1991))%string.
(*= "Current year is 11111100001 and I'm 11010 years old"*)

Require Import Coq.extraction.ExtrHaskellNatInteger.
Require Import Coq.extraction.ExtrHaskellString.
Require Import Coq.extraction.ExtrHaskellBasic.


Extraction Language Haskell.
Extraction "Printf.hs" Printf.