Require Import String.
Require Import Ascii.
Require Import ZArith.
Load Base10.
Open Scope Z.
Open Scope string.

Inductive PrintDirective :=
| FormatLit : ascii -> PrintDirective
| FormatNum : PrintDirective
| FormatString : PrintDirective
| FormatChar : PrintDirective.

Definition PrintFormat := list PrintDirective.

Definition directiveType (dir : PrintDirective) :=
match dir with
  | FormatLit _ => None
  | FormatNum => Some Z
  | FormatString => Some string
  | FormatChar => Some ascii
end.

Fixpoint formatType (f : PrintFormat) : Set :=
  match f with
    | nil => string
    | cons dir dirs =>
      match directiveType dir with
        | Some T => T -> formatType dirs
        | None => formatType dirs
      end
  end.

Fixpoint sprintf (f : PrintFormat) (k:string->string) : formatType f :=
match f with
| nil => k EmptyString
| cons dir dirs =>
  match dir with
  | FormatLit c => sprintf dirs (fun rest => k (String c rest))
  | FormatNum => fun z => sprintf dirs (fun rest => k (z_to_str z ++ rest))
  | FormatString => fun s => sprintf dirs (fun rest => k (s ++ rest))
  | FormatChar => fun c => sprintf dirs (fun rest => k (String c rest))
  end
end.

Fixpoint parse_format (fmt:string) : PrintFormat :=
match fmt with
| String "%" (String "s" xs) => cons FormatString (parse_format xs)
| String "%" (String "d" xs) => cons FormatNum (parse_format xs)
| String "%" (String "c" xs) => cons FormatChar (parse_format xs)
| String x xs => cons (FormatLit x) (parse_format xs)
| EmptyString => nil
end.

Definition printf (fmt:string) := sprintf (parse_format fmt) (fun s=>s).

Definition IsTranslation (s s':string) := formatType(parse_format s) = formatType(parse_format s').

Definition dyn_printf (rt:string) (fmt:string) : IsTranslation rt fmt -> formatType (parse_format fmt).
intros.
unfold IsTranslation in H.
rewrite <- H.
refine (printf rt).
Defined.

Definition my_str := "Num: %d, string: %s".
Axiom external_string : string.
Axiom external_string_translates : IsTranslation external_string my_str.


Definition my_print := (dyn_printf external_string "Num: %d, string: %s" external_string_translates 12 "Hello!").


Require Import Coq.extraction.ExtrHaskellNatInteger.
Require Import Coq.extraction.ExtrHaskellString.
Require Import Coq.extraction.ExtrHaskellBasic.


Extraction Language Haskell.
Extraction "my_print.hs" my_print.

