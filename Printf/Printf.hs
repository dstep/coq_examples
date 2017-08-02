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

append :: Prelude.String -> Prelude.String -> Prelude.String
append s1 s2 =
  case s1 of {
   ([]) -> s2;
   (:) c s1' -> (:) c (append s1' s2)}

type TypeForFormatString = Any

last_digit :: Prelude.Integer -> Prelude.Char
last_digit n =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ ->
    '0')
    (\n0 ->
    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
      (\_ ->
      '1')
      (\n' ->
      last_digit n')
      n0)
    n

div2 :: Prelude.Integer -> Prelude.Integer
div2 n =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ ->
    0)
    (\n0 ->
    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
      (\_ ->
      0)
      (\n' -> Prelude.succ
      (div2 n'))
      n0)
    n

nat_to_str :: Prelude.Integer -> Prelude.String
nat_to_str x =
  let {nat_to_str0 = \n -> nat_to_str (proj1_sig n)} in
  let {filtered_var = div2 x} in
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> (:) (last_digit x)
    ([]))
    (\n' ->
    append (nat_to_str0 (Prelude.succ n')) ((:) (last_digit x) ([])))
    filtered_var

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

