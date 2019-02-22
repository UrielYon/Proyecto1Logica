module Semantics where

import Syntax

type Estado = [VarP]

inter :: Estado -> Prop -> Bool
inter e phi = case phi of
  TTrue -> True
  FFalse -> False
  V x -> elem x e
  Neg p -> not (inter e p)
  Conj p q -> (inter e p) && (inter e q)
  Disy p q -> (inter e p) || (inter e q)
  Imp p q -> not (inter e p) || (inter e q)
  Equiv p q -> (inter e p) == (inter e q)

estados :: Prop -> [Estado]
estados phi = subconj (vars phi)

modelos :: Prop -> [Estado]
modelos phi = [e | e <- estados phi, (inter e phi) == True]

tautologia :: Prop -> Bool
tautologia phi = (modelos phi) == (estados phi)

satisf :: Prop -> Bool
satisf phi = modelos phi /= []

contrad :: Prop -> Bool
contrad phi = modelos phi == []

insatisfen :: Estado -> Prop -> Bool
insatisfen e phi = (inter e phi) == False

satisfen:: Estado -> Prop -> Bool
satisfen e phi = (inter e phi) == True 


equiv :: Prop -> Prop -> Bool
equiv p q = tautologia (Equiv p q)




estadosConj :: [Prop] -> [Estado]
estadosConj xs = estados (pega (xs))

modelosConj :: [Prop] ->[Estado]
modelosConj xs = modelos(pega xs)

satisfConj :: [Prop] -> Bool
satisfConj xs = satisf(pega xs)

insatisfConj :: [Prop] -> Bool
insatisfConj xs = contrad (pega xs)

satisfenConj:: Estado -> [Prop] -> Bool
satisfenConj e xs = satisfen e (pega xs)

insatisfenConj:: Estado -> [Prop] -> Bool
insatisfenConj e xs = insatisfen e (pega xs)


pega :: [Prop] -> Prop
pega [] = TTrue
pega (x:xs) = Conj x (pega xs)


consecuencia :: [Prop] -> Prop -> Bool
consecuencia gamma phi = insatisfConj((Neg phi):gamma) 

--Auxiliares
vars :: Prop -> [VarP]
vars phi = case phi of
  TTrue -> []
  FFalse -> []
  V x -> [x]
  Neg p -> vars p
  Conj p q -> union (vars p) (vars q)
  Disy p q -> union (vars p) (vars q)
  Imp p q -> union (vars p) (vars q)
  Equiv p q -> union (vars p) (vars q)


eliminaRep :: Eq a => [a] -> [a]
eliminaRep [] = []
eliminaRep (x:xs) = x : eliminaRep (filter (/=x) xs)


union :: [VarP] -> [VarP] -> [VarP]
union xs ys = eliminaRep (xs ++ ys)

subconj :: [a] -> [[a]]
subconj [] = [[]]
subconj (x:xs) = xs' ++ map  (x:) xs' where xs' = subconj xs