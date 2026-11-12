{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Prj3Sol (
  toSingletonLists,
  listMap,
  append,
  member,
  selectNApart,
  sumListsRec,
  sumListsPipe,
  evalPrj2Expr,
  EvalValue(..)
  ) where

import Data.Function ((&))
import Data.List (lookup, elemIndices)
import Text.Printf

import Prj2LangTypes

---------------------------- toSingletonLists ---------------------------

-- toSingletonLists list: Given a list of elements e, return a list containing
-- of singleton lists [e].
-- Use map with the (:) section to prepend each element to an empty array, []
toSingletonLists :: [e] -> [[e]]
toSingletonLists list = map (:[]) list

--------------------------------- listMap -------------------------------

-- listMap fn x list: Given a binary function fn:: (a -> b -> c) and
-- a x::a and a list::[b], return a list containing the results of
-- fn x e, for every element e in list.
-- Use map instead of recursion and partially apply f to create func used on each element 
listMap :: (a -> b -> c) -> a -> [b] -> [c]
listMap f a list = map (f a) list


---------------------------------- append -------------------------------


-- append ls1 ls2: return ls1 ++ ls2
-- Use foldr, which can fold a list from right (:) [] is list so should get (:) ls2 then ls1
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- Starts with ls2 and uses (:) to prepend the elements of ls1
append :: [a] -> [a] -> [a]
append ls1 ls2 = foldr (:) ls2 ls1
  
---------------------------------- member -------------------------------

-- member e list: return True iff e is a member of list.
-- foldl folds from the left
-- Accumulator I named found starts False and tracks if e was found if x equals 3
member :: Eq e => e -> [e] -> Bool
member e list = foldl (\found x -> found || e == x) False list


------------------------------- selectNApart ----------------------------

-- selectNApart n list: given an Int n and a list of elements e,
-- return a list containing the elements of the list at index 0,
-- n, 2n, 3n, 4n, ... for n > 0.
-- Keep first element then skip n-1 elements
-- Page 51 notes with drop fibonacci example
selectNApart :: Int -> [e] -> [e]
selectNApart n list = 
  case list of
    [] -> []
    (x:xs) -> x : selectNApart n (drop (n-1) xs)

------------------------------ sumListsRec ------------------------------

-- sumListsRec listOfLists n: given a list of `Int` lists,
-- return the sum of those elements of listOfLists whose length
-- is divisible by `n`.
-- Call local recursive helper count and sum elements for sublists
sumListsRec :: [[Int]] -> Int -> Int
sumListsRec listOfLists n = 
  case listOfLists of
    [] -> 0
    (xs:xss) ->
      if listLength xs `rem` n == 0
      then listSum xs + sumListsRec xss n
      else sumListsRec xss n
  where
    listLength [] = 0
    listLength (_:xs) = 1 + listLength xs

    listSum [] = 0
    listSum (x:xs) = x + listSum xs
                                  
------------------------------ sumListsPipe -----------------------------

-- sumListsPipe listOfLists n: given a list of `Int` lists,
-- return the sum of those elements of listOfLists whose length
-- is divisible by `n`.
-- Combines filter, map, and foldl and not explicit recursion

sumListsPipe :: [[Int]] -> Int -> Int
sumListsPipe listOfLists n = 
  listOfLists
  & filter (\xs -> length xs `rem` n == 0) -- Keep lists divisible by n
  & map (foldl (+) 0) -- Sum each list that qualifies
  & foldl (+) 0 -- Sum the results
                                  
  
----------------------------- evalPrj2Expr ------------------------------

-- Evaluate Prj2 expressions.  You should assume no errors in the expr.
-- The expr-types are listed in Prj2LangTypes.hs
-- Follows provided evalEnv structure

data EvalValue =
  Val Int |
  Closure Env Expr
  deriving (Eq, Show)

-- Env here maps identifier names to the evaluated vals
type Env = [(String, EvalValue)]

evalPrj2Expr :: Expr -> EvalValue
evalPrj2Expr expr =
  evalEnv expr [] -- wrapper starts with empty env
  where   
    evalEnv :: Expr -> Env -> EvalValue
    evalEnv expr env =
      case expr of
      -- Handle integers and integer lookup
        Int_ { value } -> Val value
        Id { id_ } ->
          case lookup id_ env of
            Just v -> v
            Nothing -> error $ "unknown identifier '" ++ id_ ++ "'"
        -- uses provided opfn and recursively evaluates operands
        Op2 { op, rand1, rand2 } ->
          case (evalEnv rand1 env, evalEnv rand2 env) of
            (Val v1, Val v2) -> Val (opFn op v1 v2)
            _ -> error "expected integer operands"
        -- Evaluates only selected branch
        -- Zero condition selects else_; nonzero will selects then_
        If { cond, then_, else_ } -> 
          case evalEnv cond env of
            Val 0 -> evalEnv else_ env
            Val _ -> evalEnv then_ env
            _ -> error "expected integer condition"
        -- Wrap lambda in closure with curr env
        lambda@Lambda {} -> 
          Closure env lambda  
        -- local helpers for letkind; the kind differs in which env evaluates definition
        Let { letKind, defs, expr } ->
          case letKind of
            Parallel -> evalEnv expr (parallelDefs defs env env)
            Sequential -> evalEnv expr (sequentialDefs defs env)
            Recursive ->
              let recursiveEnv = recursiveDefs defs env recursiveEnv
              in evalEnv expr recursiveEnv  
        -- Look up closure and evalute body after binding parameters
        App { name, args } ->
          case lookup name env of
            Just (Closure closureEnv (Lambda {params, body})) ->
              evalEnv body (bindArgs params args closureEnv env)
            _ -> error "expected a closure"  
        _ -> error $ "unhandled expr " -- ++ (show expr)

    ---------------- HELPERS ----------------
    -- Parallel def all evaulate in og env
    parallelDefs defs originalEnv extendedEnv = 
      case defs of
        [] -> extendedEnv
        (Def {id_, expr}:rest) ->
          parallelDefs rest originalEnv
          ((id_, evalEnv expr originalEnv) : extendedEnv)
        _ -> error "expected a definition" 
    -- SequentialDefs evaluate in the env built so far
    -- Earlier bindings can be used
    sequentialDefs :: [Expr] -> Env -> Env
    sequentialDefs defs env =
      case defs of
        [] -> env
        (Def { id_, expr}:rest) ->
          sequentialDefs rest ((id_, evalEnv expr env) : env)
        _ -> error  "expected a defintion"

    -- Recursive def to see the final environment
    -- Every definition can refer to final env and uses recursive let
    recursiveDefs :: [Expr] -> Env -> Env -> Env
    recursiveDefs defs env recursiveEnv =
      case defs of
        [] -> env
        (Def {id_, expr}:rest) ->
          (id_, evalEnv expr recursiveEnv) : recursiveDefs rest env recursiveEnv
        _ -> error "expected a definition"

    -- Parameters are added to captured closure env
    bindArgs :: [String] -> [Expr] -> Env -> Env -> Env
    bindArgs params args closureEnv callerEnv =
      case (params, args) of
        ([], []) -> closureEnv
        (param:moreParams, arg:moreArgs) ->
          (param, evalEnv arg callerEnv) :
            bindArgs moreParams moreArgs closureEnv callerEnv
        _ -> error "incorrect number of arguments"

    
    opFn op =
      case lookup op opFuncs of
        Nothing -> error $ "unknown operator '" ++ op ++ "'"
        Just fn -> fn
      where
        opFuncs = [
            ("+", \a b -> a+b),
            ("-", \a b -> a-b),
            ("*", \a b -> a*b),
            ("/", \a b -> a `div` b),
            ("%", \a b -> a `rem` b),
            ("==", \a b -> if a == b then 1  else 0),
            ("<", \a b -> if a < b then 1 else 0)
          ]
    
    
