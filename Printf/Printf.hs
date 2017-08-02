{-# OPTIONS_GHC -cpp -XMagicHash #-}
{- For Hugs, use the option -F"cpp -P -traditional" -}

module Printf where

import qualified Prelude

#ifdef __GLASGOW_HASKELL__
import qualified GHC.Base
import qualified GHC.Prim
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

type Sig a =
  a
  -- singleton inductive, whose constructor was exist
  
proj1_sig :: a1 -> a1
proj1_sig e =
  e

sub :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
sub = (\n m -> Prelude.max 0 (n Prelude.- m))

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
                        (sub n' (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                          (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ 0))))))))))))
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

type TypeForFormatString = Any

printf_ :: Prelude.String -> Prelude.String -> TypeForFormatString
printf_ s pref =
  case s of {
   ([]) -> unsafeCoerce pref;
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
         Prelude.True ->
          case b1 of {
           Prelude.True -> printf_ xs (append pref ((:) x ([])));
           Prelude.False ->
            case b2 of {
             Prelude.True -> printf_ xs (append pref ((:) x ([])));
             Prelude.False ->
              case b3 of {
               Prelude.True -> printf_ xs (append pref ((:) x ([])));
               Prelude.False ->
                case b4 of {
                 Prelude.True ->
                  case b5 of {
                   Prelude.True -> printf_ xs (append pref ((:) x ([])));
                   Prelude.False ->
                    case b6 of {
                     Prelude.True -> printf_ xs (append pref ((:) x ([])));
                     Prelude.False -> unsafeCoerce (\x0 -> printf_ xs (append pref (nat_to_str x0)))}};
                 Prelude.False -> printf_ xs (append pref ((:) x ([])))}}}};
         Prelude.False ->
          case b1 of {
           Prelude.True ->
            case b2 of {
             Prelude.True -> printf_ xs (append pref ((:) x ([])));
             Prelude.False ->
              case b3 of {
               Prelude.True -> printf_ xs (append pref ((:) x ([])));
               Prelude.False ->
                case b4 of {
                 Prelude.True ->
                  case b5 of {
                   Prelude.True -> printf_ xs (append pref ((:) x ([])));
                   Prelude.False ->
                    case b6 of {
                     Prelude.True -> printf_ xs (append pref ((:) x ([])));
                     Prelude.False -> unsafeCoerce (\s0 -> printf_ xs (append pref s0))}};
                 Prelude.False -> printf_ xs (append pref ((:) x ([])))}}};
           Prelude.False -> printf_ xs (append pref ((:) x ([])))}};
       Prelude.False -> printf_ xs (append pref ((:) x ([])))})
      x}

printf :: Prelude.String -> TypeForFormatString
printf s =
  printf_ s ([])

