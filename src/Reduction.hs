module Reduction (reduce) where

import Control.Monad (when)
import Data.Char (toLower)
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Maybe qualified as Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Syntax (Substitution, Term (..), Var)

-- a stock of variablenames that is used for getting fresh variables
vars :: [Var]
vars = ['v' : '$' : show n | n <- [0 :: Int ..]]

-- free variables
fv :: Term -> Set Var
fv (Var v) = Set.singleton v
fv (App t1 t2) = Set.union (fv t1) (fv t2)
fv (Abs v t) = Set.difference (fv t) (Set.singleton v)

-- capture avoiding substitution
substT :: Substitution -> Term -> Term
substT o (Var x) = o x
substT o (App t1 t2) = App (substT o t1) (substT o t2)
substT o (Abs x t) = Abs y (substT o' t)
  where
    y = if isFresh x then x else fromMaybe (error "Error: no fresh variable found, cant happen!") $ find isFresh vars
    o' v
      | v == x = Var y
      | otherwise = o v
    zs = Set.map o $ fv (Abs x t)
    isFresh :: Var -> Bool
    isFresh e = not (any (Set.member e . fv) zs)

-- do a single reduction step, implements 'normal-reduction'
reduceStep :: Map String Term -> Term -> (Reduction, Term)
-- abstraction: reduce the inner term
reduceStep globals (Abs x t) = let (r, t') = reduceStep globals t in (r, Abs x t')
-- abstraction applied to term - beta reduction!
reduceStep globals (App (Abs x t) s) = (Beta, substT (\y -> if x == y then s else Var y) t)
-- application, here we either reduce the left or right term
reduceStep globals (App t s)
  | t' == t = (rs, App t s') -- t is normal, reduce s
  | otherwise = (rt, App t' s) -- t is not normal, reduce t
  where
    (rt, t') = reduceStep globals t
    (rs, s') = reduceStep globals s
-- variable, here we either delta-reduce (if name is known) or stop
reduceStep globals t@(Var x)
  | x `Map.member` globals = (Delta, globals Map.! x)
  | otherwise = (None, t)

reduce :: Int -> Bool -> Map String Term -> Term -> IO ()
reduce 1000000 full m t = do
  putStrLn "Evaluation reached 1.000.000 reduction steps, this might be an endless loop.\nIf you want the evaluation to continue type Y, else type something else."
  line <- getLine
  case line of
    "Y" -> putStrLn "Continuing evaluation..." >> reduce 1000001 full m t
    _ -> putStrLn "Evaluation cancelled"
reduce count full m t = do
  let (r, t') = reduceStep m t
  when full $ do
    case r of
      Beta -> putStr "-β-> " >> print t'
      Delta -> putStr "-δ-> " >> print t'
      None -> showResult m t'
  if r == None then showResult m t' else reduce (count + 1) full m t'

showResult :: Map String Term -> Term -> IO ()
showResult globals t = do
  putStr "result: "
  print t
  case parseChurchNumeral t of
    Nothing -> return ()
    Just x -> putStr "as church-numeral: " >> print x
  case parseChurchBoolean t of
    Nothing -> return ()
    Just x -> putStr "as church-boolean: " >> putStrLn (map toLower $ show x)
  case parseFoldList t of
    Nothing -> return ()
    Just x -> do
      let numeralList = mapM parseChurchNumeral x
      case numeralList of
        Nothing -> return ()
        Just x -> putStr "as fold-list (numeral):" >> print x
      let booleanList = mapM parseChurchBoolean x
      case booleanList of
        Nothing -> return ()
        Just x -> putStr "as fold-list (boolean):" >> print x
  putStrLn ""

parseChurchNumeral :: Term -> Maybe Int
parseChurchNumeral (Abs f (Abs a (Var a'))) | a == a' = Just 0
parseChurchNumeral (Abs x (Abs y t)) = if count == Just 0 then Nothing else count
  where
    count = countApps t
    countApps :: Term -> Maybe Int
    countApps (App t1 t2) = (+ 1) <$> countApps t2
    countApps _ = Just 0
parseChurchNumeral _ = Nothing

parseChurchBoolean :: Term -> Maybe Bool
parseChurchBoolean (Abs x (Abs y (Var x'))) | x == x' = Just True
parseChurchBoolean (Abs x (Abs y (Var y'))) | y == y' = Just False
parseChurchBoolean _ = Nothing

parseFoldList :: Term -> Maybe [Term]
parseFoldList (Abs c (Abs n (Var n'))) | n == n' = Just []
parseFoldList (Abs c (Abs n (App (App (Var c') e) t))) | c == c' = (:) e <$> parseRest c n t
  where
    parseRest :: String -> String -> Term -> Maybe [Term]
    parseRest c n (App (App (Var c') e) (Var n')) | c == c' && n == n' = Just [e]
    parseRest c n (App (App (Var c') e) t) | c == c' = (:) e <$> parseRest c n t
    parseRest c n _ = Nothing
parseFoldList _ = Nothing

data Reduction = Beta | Delta | None deriving (Eq)
