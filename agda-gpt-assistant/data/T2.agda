module T2 where

open import Data.Nat

data List (A : Set) : Set where
  nil : List A
  cons : A → List A → List A

length : {A : Set} → List A → Nat
length nil = zero
length (cons _ xs) = suc (length xs)
