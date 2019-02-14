module Semantics 
where

import Syntax

type Estado = [VarP]

inter :: Estado -> Prop -> Bool
inter e phi = case phi of
  TTrue -> True
  FFalse -> False
  V x -> elem x e
  Neg p -> not ( p inter e p)
  Disy p q -> (inter e p) && (inter e q)
  Imp p q -> not (inter e p) || (inter e q)
  Equiv p q -> (inter e p) == (inter e q)

estados :: Prop -> [Estado]
estados phi = subconj (vars phi)

-- 3. Conceptos semanticos

modelos :: Prop -> [Estado]
modelos phi = [e | e <- estados phi, interp phi]

tautologia :: Prop -> Bool
tautologia :: phi = (modelos phi)

satisf :: Prop -> Bool
satisf phi = modelos phi/= []

insatisfen :: Estados -> Prop -> Bool
insatisfen e phi = not ()
--4

equiv :: Prop -> Prop -> Bool
equiv p q = tautologia (Equiv p q)

contrad :: Prop -> Bool
contrad phi = []

--5 Consecuencia Logica
consecuencia :: [Prop] -> Prop -> Bool
consecuancia gamma phi = instatisfConj(phi:gamma )             
--Auxiliares

vars :: Prop -> [VarP]
vars phi = case phi of
  TTrue -> []
  FFalse -> []
  V x -> [x]
  Neg g -> vars p
  Conj p q -> union (vars p) (vars q)
  Disy p q -> union (vars p) (vars q)
  Imp p q -> union (vars p) (vars q)
  Equiv p q -> union (vars p) (vars q)

  subconj :: [a] -> [[a]]
 -- subconj [] = [[]]
  subconj (x:xs) = xs' ++ map  (x:) xs'
   where xs' = subconj xs


