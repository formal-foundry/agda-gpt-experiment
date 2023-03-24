data Nat : Set where
zero : Nat  
suc : Nat \8594 Nat

add : Nat \8594 Nat \8594 Nat
add zero m = m
add (suc n) m = suc (add n m)
