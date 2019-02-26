module Tests where

import Syntax
import Semantics

quita :: [a] -> Int -> [a]
quita l n = case l of
  [] -> []
  x:xs -> case n of
    0 -> xs
    _ -> x:(quita xs (n - 1))

main = do
  let a = V 1
  let na = Neg a
  let b = V 2
  let nb = Neg b
  let c = V 3
  let nc = Neg c
  let d = V 4
  let nd = Neg d
  let p1 = Disy na (Disy b d)
  let p2 = Disy nb (Disy c d)
  let p3 = Disy nc (Disy a d)
  let p4 = Disy a (Disy nb nd)
  let p5 = Disy b (Disy nc nd)
  let p6 = Disy na (Disy c nd)
  let p7 = Disy a (Disy b c)
  let p8 = Disy na (Disy nb nc)
  
  let conj = [p1, p2, p3, p4, p5, p6, p7, p8]
  -- Deberia imprimir False
  print $satisfConj conj

  let satConjs = [quita conj x | x <- [0..7]]
  -- Deberia imprimir puros True
  print $map satisfConj satConjs

  let v = [V x | x <- [1..16]]
  let f = Disy (Conj (V 1) (V 2)) (Disy (Conj (V 3) (V 4)) (Conj (V 5) (V 6)))
  print f
  -- Revisa que la formula efectivamente este en FNC, o implementa una
  -- funcion que lo cheque por ti ;)
  print $fnc f
  
  let g = Disy (Disy (Disy a b) (Conj c d)) (Disy a c)
  print g
  print $fnc g
