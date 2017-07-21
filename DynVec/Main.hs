{-# LANGUAGE FlexibleInstances #-}

module Main where

import DynVec

instance Show (Vec Nat) where
	show (Vec_empty) = "[]"
	show (Vec_ex _ xs x) = show (nat_to_int x) ++ " :: " ++ show xs

splitInput ss = map (read::String->Integer) (words ss)

nat_to_int O = 0
nat_to_int (S n) = (nat_to_int n) + 1
int_to_nat 0 = O
int_to_nat x = S (int_to_nat (x - 1))

streamify (x:xs) = Stream_next (int_to_nat x) (streamify xs)

main = do
	input <- getLine  
	let stream = splitInput input
	let (ExistT _ v) = my_main (streamify stream)
	print v