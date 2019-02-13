module Syntax where

-- | VarP. Tipo que representa las variables proposicionales
type VarP = Int

-- | Prop. Tipo que representa formulas de logica proposicional
data Prop = TTrue
          | FFalse
          | V VarP
          | Neg Prop
          | Conj Prop Prop
          | Disy Prop Prop
          | Imp Prop Prop
          | Equiv Prop Prop

