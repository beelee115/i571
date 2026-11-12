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


{-
  YOU WILL RECEIVE A SUBSTANTIAL PENALTY (INCLUDING POSSIBLY A ZERO
  FOR THE ENTIRE PROJECT) IF YOU DO NOT MEET THE FOLLOWING
  RESTRICTIONS:

    + You may not use any "advanced" Haskell builtins or features not
      covered in class or mentioned explicitly in this project.  Note
      that a builtin function fn is regarded as covered in class if it
      has been used in class with the same name fn or defined in class
      with name myFn or my_fn.

    + You should not define any top-level functions other than
      those listed below.  Note that you can use `where` or
      `let` to implement local functions (which can even close
      over the parameter and locals of the enclosing function).

    + If an exercise specifies a restriction on the implementation,
      then your implementation must meet that restriction.

    + Your submission must not contain garbage files.  Specifically,
      it should unpack into a prj3-sol directory and contain only this
      file and your README.      

  Note that a function is allowed to call a function defined earlier.
-}

---------------------------- toSingletonLists ---------------------------

-- #1: 5-points

-- toSingletonLists list: Given a list of elements e, return a list containing
-- of singleton lists [e].
-- *Restriction*: must use map with a section.
-- map applies a function specified by first argument to each element of list
toSingletonLists :: [e] -> [[e]]
toSingletonLists list = map (:[]) list

--------------------------------- listMap -------------------------------

-- #2: 5-points

-- listMap fn x list: Given a binary function fn:: (a -> b -> c) and
-- a x::a and a list::[b], return a list containing the results of
-- fn x e, for every element e in list.
-- *Restriction*: cannot use explicit recursion
-- Hint: use the map function or a list comprehension
listMap :: (a -> b -> c) -> a -> [b] -> [c]
listMap f a list = map (f a) list


---------------------------------- append -------------------------------

-- #3: 10-points

-- append ls1 ls2: return ls1 ++ ls2
-- Restriction: must use either foldl or foldr; cannot use ++ or recursion
-- foldr folds a list from right (:) [] is list so should get (:) ls2 then ls1
-- foldr :: (a -> b -> b) -> b -> [a] -> b
append :: [a] -> [a] -> [a]
append ls1 ls2 = foldr (:) ls2 ls1
  
---------------------------------- member -------------------------------

-- #4: 10-points

-- member e list: return True iff e is a member of list.
-- *Restriction*: must use foldl, cannot use elem.
-- Hint: define folding function using a lambda or local let/where definition;
-- also see recursive definition of member in slides.
-- folds from the left
member :: Eq e => e -> [e] -> Bool
member e list = foldl (\found x -> found || e == x) False list


------------------------------- selectNApart ----------------------------

-- #5: 10-points

-- selectNApart n list: given an Int n and a list of elements e,
-- return a list containing the elements of the list at index 0,
-- n, 2n, 3n, 4n, ... for n > 0.
-- keep first element then skip n-1 elements
-- page 51 notes with drop fibonacci example
-- Hint: use drop
selectNApart :: Int -> [e] -> [e]
selectNApart n list = 
  case list of
    [] -> []
    (x:xs) -> x : selectNApart n (drop (n-1) xs)

------------------------------ sumListsRec ------------------------------

-- #6: 10-points

-- sumListsRec listOfLists n: given a list of `Int` lists,
-- return the sum of those elements of listOfLists whose length
-- is divisible by `n`.

-- *Restriction*: must be implemented using recursion.  Can use
-- only basic arithmetic operators.  Cannot call any other external
-- functions.
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

-- #7: 10-points

-- sumListsPipe listOfLists n: given a list of `Int` lists,
-- return the sum of those elements of listOfLists whose length
-- is divisible by `n`.

-- *Restriction*: must be implemented using the `&` pipe operator.
-- Cannot define any auxiliary functions, but can use `map`, `filter`
-- `foldl` and `foldr`.
sumListsPipe :: [[Int]] -> Int -> Int
sumListsPipe listOfLists n = 
  listOfLists
  & filter (\xs -> length xs `rem` n == 0)
  & map (foldl (+) 0)
  & foldl (+) 0
                                  
  
----------------------------- evalPrj2Expr ------------------------------

-- #8: 40-points

-- Evaluate Prj2 expressions.  You should assume no errors in the expr.
-- The expr-types are listed in Prj2LangTypes.hs
--
-- Hints provided in write-up

data EvalValue =
  Val Int |
  Closure Env Expr
  deriving (Eq, Show)
  
type Env = [(String, EvalValue)]

evalPrj2Expr :: Expr -> EvalValue
evalPrj2Expr expr =
  evalEnv expr []
  where   
    evalEnv :: Expr -> Env -> EvalValue
    evalEnv expr env =
      case expr of
        Int_ { value } -> Val value
        Id { id_ } ->
          case lookup id_ env of
            Just v -> v
            Nothing -> error $ "unknown identifier '" ++ id_ ++ "'"
        Op2 { op, rand1, rand2 } ->
          case (evalEnv rand1 env, evalEnv rand2 env) of
            (Val v1, Val v2) -> Val (opFn op v1 v2)
            _ -> error "expected integer operands"
        If { cond, then_, else_ } -> 
          case evalEnv cond env of
            Val 0 -> evalEnv else_ env
            Val _ -> evalEnv then_ env
            _ -> error "expected integer condition"
        lambda@Lambda {} -> 
          Closure env lambda  
        Let { letKind, defs, expr } ->
          case letKind of
            Parallel -> evalEnv expr (parallelDefs defs env env)
            Sequential -> evalEnv expr (sequentialDefs defs env)
            Recursive ->
              let recursiveEnv = recursiveDefs defs env recursiveEnv
              in evalEnv expr recursiveEnv  
        App { name, args } ->
          case lookup name env of
            Just (Closure closureEnv (Lambda {params, body})) ->
              evalEnv body (bindArgs params args closureEnv env)
            _ -> error "expected a closure"  
        _ -> error $ "unhandled expr " -- ++ (show expr)

    -- TODO: add more aux fns
    -- Parallel def all evaulate in og env
    parallelDefs defs originalEnv extendedEnv = 
      case defs of
        [] -> extendedEnv
        (Def {id_, expr}:rest) ->
          parallelDefs rest originalEnv
          ((id_, evalEnv expr originalEnv) : extendedEnv)
        _ -> error "expected a definition" 
    -- SequentialDefs evaluate in the env built so far
    sequentialDefs :: [Expr] -> Env -> Env
    sequentialDefs defs env =
      case defs of
        [] -> env
        (Def { id_, expr}:rest) ->
          sequentialDefs rest ((id_, evalEnv expr env) : env)
        _ -> error  "expected a defintion"

    -- Recursive def to see the final environment
    recursiveDefs :: [Expr] -> Env -> Env -> Env
    recursiveDefs defs env recursiveEnv =
      case defs of
        [] -> env
        (Def {id_, expr}:rest) ->
          (id_, evalEnv expr recursiveEnv) : recursiveDefs rest env recursiveEnv
        _ -> error "expected a definition"

    -- parameters are added to captured closure env
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
    
    
