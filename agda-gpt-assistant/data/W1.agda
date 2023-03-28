data List (A : Set) : Set where
  nil : List A
  cons : A → List A → List A

data Nat : Set where
  zero : Nat
  suc  : Nat → Nat




length : {A : Set} → List A → Nat
length {A} nil = zero
length {A} (cons _ xs) = suc (length xs)
