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

instance Show Prop where
  show phi = case phi of
    TTrue -> "T"
    FFalse -> "F"
    V x -> show x
    Neg p -> "(~" ++ show p ++ ")"
    Conj p q -> "(" ++ show p ++ " ^ " ++ show q ++ ")"
    Disy p q -> "(" ++ show p ++ " v " ++ show q ++ ")"
    Imp p q -> "(" ++ show p ++ " -> " ++ show q ++ ")"
    Equiv p q -> "(" ++ show p ++ " <-> " ++ show q ++ ")"


eliminaEq :: Prop -> Prop
eliminaEq phi = case phi of
	Neg p -> Neg(eliminaEq p)
	Conj p q -> (Conj (eliminaEq p) (eliminaEq q))
	Disy p q -> (Disy (eliminaEq p) (eliminaEq q))
	Imp p q -> (Imp (eliminaEq p) (eliminaEq q))
	Equiv p q -> (Conj (Imp p' q') (Imp p' q'))
		where p' = eliminaEq p
		      q' = eliminaEq q

	_ -> phi

eliminaImp :: Prop -> Prop
eliminaImp phi = case phi of
	Neg p -> Neg(eliminaImp p)
	Conj p q -> (Conj (eliminaImp p) (eliminaImp q))
	Disy p q -> (Disy (eliminaImp p) (eliminaImp q))
	Imp p q -> (Disy (Neg (eliminaImp p)) (eliminaImp q))
	Equiv p q -> (Equiv (eliminaImp p) (eliminaImp q))
	_ -> phi

meteNeg :: Prop -> Prop
meteNeg phi = case phi of
	TTrue -> TTrue
	FFalse -> FFalse
	V x -> V x
	Neg psi -> case psi of
		Conj p q -> Disy (meteNeg (Neg p)) (meteNeg(Neg q))
		Disy p q -> Conj (meteNeg (Neg p)) (meteNeg (Neg q))
		Neg p -> meteNeg p
		TTrue -> FFalse
		FFalse -> TTrue
		V x -> Neg(V x)
	Conj p q -> Conj (meteNeg p) (meteNeg q)
	Disy p q -> Disy (meteNeg p) (meteNeg q)

fnn :: Prop -> Prop
fnn = meteNeg.eliminaImp.eliminaEq

dist :: Prop -> Prop
dist phi = case phi of
	TTrue -> TTrue
	FFalse -> FFalse
	V x -> V x
	Neg p -> Neg p
	Conj p q -> Conj (dist p) (dist q)
	Disy p (Conj q r) -> Conj (dist (Disy p q)) (dist (Disy p r))
	Disy (Conj p q) r -> Conj (dist (Disy p r)) (dist (Disy q r))
	Disy p q -> case (p' , q') of 
		(Conj _ _, _) -> dist (Disy p' q')
		(_, Conj _ _) -> dist (Disy p' q')
		(_,_) -> Disy p' q'
		where p' = dist p
		      q' = dist q

fnc :: Prop -> Prop
fnc = dist.fnn

		

