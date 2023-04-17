module Syntax where

type Var = String
type Substitution = Var -> Term

data Declaration = Declaration String Term

data Term
  = Var Var
  | App Term Term
  | Abs Var Term
  deriving (Eq, Ord)

instance Show Term where
  show :: Term -> String
  show (Var v) = v
  show (App t1 t2) = showHelper t1 False ++ " " ++ showHelper t2 True
    where
      showHelper :: Term -> Bool -> String
      showHelper (Var v) True = v
      showHelper (App t1 t2) True = "(" ++ showHelper t1 False ++ " " ++ showHelper t2 True ++ ")"
      showHelper t@(Abs {}) True = "(" ++ show t ++ ")"
      showHelper t False = show t
  show t@(Abs {}) = absHelper t True -- only outermost abstraction needs parentheses
    where
      absHelper :: Term -> Bool -> String
      absHelper (Abs v t) True = "(\\" ++ v ++ "." ++ absHelper t False ++ ")"
      absHelper (Abs v t) False = "\\" ++ v ++ "." ++ absHelper t False
      absHelper t _ = show t

instance Show Declaration where
  show :: Declaration -> String
  show (Declaration name term) = name ++ " = " ++ show term
