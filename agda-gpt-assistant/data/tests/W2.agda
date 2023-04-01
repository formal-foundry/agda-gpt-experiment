data List (A : Set) : Set where
  nil : List A
  cons : A → List A → List A


 
not : Bool → Bool
not true = false
not false = true
