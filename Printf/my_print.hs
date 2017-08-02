{-# OPTIONS_GHC -cpp -XMagicHash #-}
{- For Hugs, use the option -F"cpp -P -traditional" -}

module My_print where

import qualified Prelude

#ifdef __GLASGOW_HASKELL__
import qualified GHC.Base
import qualified GHC.Prim
import qualified Data.Bits
import qualified Data.Char
#else
-- HUGS
import qualified IOExts
#endif

#ifdef __GLASGOW_HASKELL__
unsafeCoerce :: a -> b
unsafeCoerce = GHC.Base.unsafeCoerce#
#else
-- HUGS
unsafeCoerce :: a -> b
unsafeCoerce = IOExts.unsafeCoerce
#endif

#ifdef __GLASGOW_HASKELL__
type Any = GHC.Prim.Any
#else
-- HUGS
type Any = ()
#endif

__ :: any
__ = Prelude.error "Logical or arity value used"

eq_rect :: a1 -> a2 -> a1 -> a2
eq_rect _ f _ =
  f

eq_rec :: a1 -> a2 -> a1 -> a2
eq_rec x f y =
  eq_rect x f y

type Sig a =
  a
  -- singleton inductive, whose constructor was exist
  
proj1_sig :: a1 -> a1
proj1_sig e =
  e

sub :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
sub = (\n m -> Prelude.max 0 (n Prelude.- m))

data Positive =
   XI Positive
 | XO Positive
 | XH

data Z =
   Z0
 | Zpos Positive
 | Zneg Positive

iter_op :: (a1 -> a1 -> a1) -> Positive -> a1 -> a1
iter_op op p a =
  case p of {
   XI p0 -> op a (iter_op op p0 (op a a));
   XO p0 -> iter_op op p0 (op a a);
   XH -> a}

to_nat :: Positive -> Prelude.Integer
to_nat x =
  iter_op (Prelude.+) x (Prelude.succ 0)

append :: Prelude.String -> Prelude.String -> Prelude.String
append s1 s2 =
  case s1 of {
   ([]) -> s2;
   (:) c s1' -> (:) c (append s1' s2)}

last_digit_to_char :: Prelude.Integer -> Prelude.Char
last_digit_to_char x =
  let {last_digit_to_char0 = \n -> last_digit_to_char (proj1_sig n)} in
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ ->
    '0')
    (\n ->
    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
      (\_ ->
      '1')
      (\n0 ->
      (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
        (\_ ->
        '2')
        (\n1 ->
        (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
          (\_ ->
          '3')
          (\n2 ->
          (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
            (\_ ->
            '4')
            (\n3 ->
            (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
              (\_ ->
              '5')
              (\n4 ->
              (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
                (\_ ->
                '6')
                (\n5 ->
                (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
                  (\_ ->
                  '7')
                  (\n6 ->
                  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
                    (\_ ->
                    '8')
                    (\n7 ->
                    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
                      (\_ ->
                      '9')
                      (\n8 ->
                      let {
                       n' = Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ n8)))))))))}
                      in
                      last_digit_to_char0
                        (sub n' (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                          (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ 0))))))))))))
                      n7)
                    n6)
                  n5)
                n4)
              n3)
            n2)
          n1)
        n0)
      n)
    x

div10 :: Prelude.Integer -> Prelude.Integer
div10 n =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ ->
    0)
    (\n0 ->
    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
      (\_ ->
      0)
      (\n1 ->
      (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
        (\_ ->
        0)
        (\n2 ->
        (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
          (\_ ->
          0)
          (\n3 ->
          (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
            (\_ ->
            0)
            (\n4 ->
            (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
              (\_ ->
              0)
              (\n5 ->
              (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
                (\_ ->
                0)
                (\n6 ->
                (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
                  (\_ ->
                  0)
                  (\n7 ->
                  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
                    (\_ ->
                    0)
                    (\n8 ->
                    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
                      (\_ ->
                      0)
                      (\n' -> Prelude.succ
                      (div10 n'))
                      n8)
                    n7)
                  n6)
                n5)
              n4)
            n3)
          n2)
        n1)
      n0)
    n

nat_to_str :: Prelude.Integer -> Prelude.String
nat_to_str x =
  let {nat_to_str0 = \n -> nat_to_str (proj1_sig n)} in
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> (:) (last_digit_to_char x)
    ([]))
    (\n ->
    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
      (\_ -> (:) (last_digit_to_char x)
      ([]))
      (\n0 ->
      (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
        (\_ -> (:) (last_digit_to_char x)
        ([]))
        (\n1 ->
        (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
          (\_ -> (:) (last_digit_to_char x)
          ([]))
          (\n2 ->
          (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
            (\_ -> (:) (last_digit_to_char x)
            ([]))
            (\n3 ->
            (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
              (\_ -> (:) (last_digit_to_char x)
              ([]))
              (\n4 ->
              (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
                (\_ -> (:) (last_digit_to_char x)
                ([]))
                (\n5 ->
                (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
                  (\_ -> (:) (last_digit_to_char x)
                  ([]))
                  (\n6 ->
                  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
                    (\_ -> (:) (last_digit_to_char x)
                    ([]))
                    (\n7 ->
                    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
                      (\_ -> (:) (last_digit_to_char x)
                      ([]))
                      (\n8 ->
                      let {
                       n' = Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ n8)))))))))}
                      in
                      append (nat_to_str0 (div10 n')) ((:) (last_digit_to_char x) ([])))
                      n7)
                    n6)
                  n5)
                n4)
              n3)
            n2)
          n1)
        n0)
      n)
    x

z_to_str :: Z -> Prelude.String
z_to_str z =
  case z of {
   Z0 -> (:) '0' ([]);
   Zpos x -> nat_to_str (to_nat x);
   Zneg x -> (:) '-' (nat_to_str (to_nat x))}

data PrintDirective =
   FormatLit Prelude.Char
 | FormatNum
 | FormatString
 | FormatChar

type PrintFormat = ([]) PrintDirective

type FormatType = Any

sprintf :: PrintFormat -> (Prelude.String -> Prelude.String) -> FormatType
sprintf f k =
  case f of {
   ([]) -> unsafeCoerce k ([]);
   (:) dir dirs ->
    case dir of {
     FormatLit c -> sprintf dirs (\rest -> k ((:) c rest));
     FormatNum -> unsafeCoerce (\z -> sprintf dirs (\rest -> k (append (z_to_str z) rest)));
     FormatString -> unsafeCoerce (\s -> sprintf dirs (\rest -> k (append s rest)));
     FormatChar -> unsafeCoerce (\c -> sprintf dirs (\rest -> k ((:) c rest)))}}

parse_format :: Prelude.String -> PrintFormat
parse_format fmt =
  case fmt of {
   ([]) -> ([]);
   (:) x xs ->
    (\f a -> f (Data.Bits.testBit (Data.Char.ord a) 0)
              (Data.Bits.testBit (Data.Char.ord a) 1)
              (Data.Bits.testBit (Data.Char.ord a) 2)
              (Data.Bits.testBit (Data.Char.ord a) 3)
              (Data.Bits.testBit (Data.Char.ord a) 4)
              (Data.Bits.testBit (Data.Char.ord a) 5)
              (Data.Bits.testBit (Data.Char.ord a) 6)
              (Data.Bits.testBit (Data.Char.ord a) 7))
      (\b b0 b1 b2 b3 b4 b5 b6 ->
      case b of {
       Prelude.True ->
        case b0 of {
         Prelude.True -> (:) (FormatLit x) (parse_format xs);
         Prelude.False ->
          case b1 of {
           Prelude.True ->
            case b2 of {
             Prelude.True -> (:) (FormatLit x) (parse_format xs);
             Prelude.False ->
              case b3 of {
               Prelude.True -> (:) (FormatLit x) (parse_format xs);
               Prelude.False ->
                case b4 of {
                 Prelude.True ->
                  case b5 of {
                   Prelude.True -> (:) (FormatLit x) (parse_format xs);
                   Prelude.False ->
                    case b6 of {
                     Prelude.True -> (:) (FormatLit x) (parse_format xs);
                     Prelude.False ->
                      case xs of {
                       ([]) -> (:) (FormatLit x) (parse_format xs);
                       (:) a xs0 ->
                        (\f a -> f (Data.Bits.testBit (Data.Char.ord a) 0)
              (Data.Bits.testBit (Data.Char.ord a) 1)
              (Data.Bits.testBit (Data.Char.ord a) 2)
              (Data.Bits.testBit (Data.Char.ord a) 3)
              (Data.Bits.testBit (Data.Char.ord a) 4)
              (Data.Bits.testBit (Data.Char.ord a) 5)
              (Data.Bits.testBit (Data.Char.ord a) 6)
              (Data.Bits.testBit (Data.Char.ord a) 7))
                          (\b7 b8 b9 b10 b11 b12 b13 b14 ->
                          case b7 of {
                           Prelude.True ->
                            case b8 of {
                             Prelude.True ->
                              case b9 of {
                               Prelude.True -> (:) (FormatLit x) (parse_format xs);
                               Prelude.False ->
                                case b10 of {
                                 Prelude.True -> (:) (FormatLit x) (parse_format xs);
                                 Prelude.False ->
                                  case b11 of {
                                   Prelude.True ->
                                    case b12 of {
                                     Prelude.True ->
                                      case b13 of {
                                       Prelude.True ->
                                        case b14 of {
                                         Prelude.True -> (:) (FormatLit x) (parse_format xs);
                                         Prelude.False -> (:) FormatString (parse_format xs0)};
                                       Prelude.False -> (:) (FormatLit x) (parse_format xs)};
                                     Prelude.False -> (:) (FormatLit x) (parse_format xs)};
                                   Prelude.False ->
                                    case b12 of {
                                     Prelude.True ->
                                      case b13 of {
                                       Prelude.True ->
                                        case b14 of {
                                         Prelude.True -> (:) (FormatLit x) (parse_format xs);
                                         Prelude.False -> (:) FormatChar (parse_format xs0)};
                                       Prelude.False -> (:) (FormatLit x) (parse_format xs)};
                                     Prelude.False -> (:) (FormatLit x) (parse_format xs)}}}};
                             Prelude.False -> (:) (FormatLit x) (parse_format xs)};
                           Prelude.False ->
                            case b8 of {
                             Prelude.True -> (:) (FormatLit x) (parse_format xs);
                             Prelude.False ->
                              case b9 of {
                               Prelude.True ->
                                case b10 of {
                                 Prelude.True -> (:) (FormatLit x) (parse_format xs);
                                 Prelude.False ->
                                  case b11 of {
                                   Prelude.True -> (:) (FormatLit x) (parse_format xs);
                                   Prelude.False ->
                                    case b12 of {
                                     Prelude.True ->
                                      case b13 of {
                                       Prelude.True ->
                                        case b14 of {
                                         Prelude.True -> (:) (FormatLit x) (parse_format xs);
                                         Prelude.False -> (:) FormatNum (parse_format xs0)};
                                       Prelude.False -> (:) (FormatLit x) (parse_format xs)};
                                     Prelude.False -> (:) (FormatLit x) (parse_format xs)}}};
                               Prelude.False -> (:) (FormatLit x) (parse_format xs)}}})
                          a}}};
                 Prelude.False -> (:) (FormatLit x) (parse_format xs)}}};
           Prelude.False -> (:) (FormatLit x) (parse_format xs)}};
       Prelude.False -> (:) (FormatLit x) (parse_format xs)})
      x}

printf :: Prelude.String -> FormatType
printf fmt =
  sprintf (parse_format fmt) (\s -> s)

dyn_printf :: Prelude.String -> Prelude.String -> FormatType
dyn_printf rt _ =
  eq_rec __ (printf rt) __

external_string :: Prelude.String
external_string = "This is my translation: (%d, %s)"

my_print :: Prelude.String
my_print =
  unsafeCoerce dyn_printf external_string ((:) 'N' ((:) 'u' ((:) 'm' ((:) ':' ((:) ' ' ((:) '%' ((:) 'd' ((:) ','
    ((:) ' ' ((:) 's' ((:) 't' ((:) 'r' ((:) 'i' ((:) 'n' ((:) 'g' ((:) ':' ((:) ' ' ((:) '%' ((:) 's'
    ([])))))))))))))))))))) (Zpos (XO (XO (XI XH)))) ((:) 'H' ((:) 'e' ((:) 'l' ((:) 'l' ((:) 'o' ((:) '!'
    ([])))))))

