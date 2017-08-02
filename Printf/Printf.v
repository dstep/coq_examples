(* Standard library for strings *)
Require Import String.
Require Import Ascii.

Require Import ZArith.
Load Base10.


(* So "xyz" notation is a string now *)
Open Scope string.

(* Gets a format string and returns a type of the printf function with that format string *)
Fixpoint TypeForFormatString (s:string) : Type :=
match s with
| EmptyString => string
| String "#" xs => Z-> TypeForFormatString xs
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

Goal (TypeForFormatString "String and two numbers: % (#, #)" = (string->Z->Z->string)). 
cbv; auto.
Qed.

(* The core printf function. Notice how its return type depends on the parameter s *)
Fixpoint Printf_ (s:string) (pref:string) : TypeForFormatString(s) :=
match s as s' return TypeForFormatString(s') with
  (*With a dependent type, you need to state upfront what type match must return*)
| EmptyString => pref
| String "#" xs => fun x => Printf_ xs (pref ++ z_to_str x)
| String "%" xs => fun s => Printf_ xs (pref ++ s)
| String x xs => Printf_ xs (pref ++ String x EmptyString)
end.

(* Wrapper for printf *)
Definition Printf (s:string) := Printf_ s ""%string.

(* A few tests *)
Eval compute in (Printf "My format string: % % %" "test" "q" "w")%string.
(* = "My format string: test q w" *)
Eval compute in (Printf "Current year is # and I'm # years old" (2017) (2017-1991))%string%Z.
(*= "Current year is 2017 and I'm 26 years old"*)

Require Import Coq.extraction.ExtrHaskellNatInteger.
Require Import Coq.extraction.ExtrHaskellString.
Require Import Coq.extraction.ExtrHaskellBasic.


Extraction Language Haskell.
Extraction "Printf.hs" Printf.