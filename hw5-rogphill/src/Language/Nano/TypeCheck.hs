{-# LANGUAGE FlexibleInstances, OverloadedStrings, BangPatterns #-}

module Language.Nano.TypeCheck where

import Language.Nano.Types
import Language.Nano.Parser

import qualified Data.List as L
import           Text.Printf (printf)  
import           Control.Exception (throw)

--------------------------------------------------------------------------------
typeOfFile :: FilePath -> IO Type
typeOfFile f = readFile f >>= typeOfString

typeOfString :: String -> IO Type
typeOfString s = typeOfExpr (parseExpr s)

typeOfExpr :: Expr -> IO Type
typeOfExpr e = do
  let (!st, t) = infer initInferState preludeTypes e
  if (length (stSub st)) < 0 then throw (Error ("count Negative: " ++ show (stCnt st)))
  else return t

--------------------------------------------------------------------------------
-- Problem 1: Warm-up
--------------------------------------------------------------------------------
-- Used UCSD CE 131 lecture notes for this assignment as a massive resource, as well as UCSD lecture slides. 

-- | Things that have free type variables
class HasTVars a where
  freeTVars :: a -> [TVar]

-- | Type variables of a type
instance HasTVars Type where
  freeTVars (TVar t)        = [t]
  freeTVars (x :=> y)       = L.nub ((freeTVars x) ++ (freeTVars y))
  freeTVars (TList i)       = freeTVars i
  freeTVars _               = []

-- | Free type variables of a poly-type (remove forall-bound vars)
instance HasTVars Poly where
  freeTVars (Forall x t)     = (freeTVars t) L.\\ [x]
  freeTVars (Mono m)         = freeTVars m

-- | Free type variables of a type environment
instance HasTVars TypeEnv where
  freeTVars gamma   = concat [freeTVars s | (x, s) <- gamma]  
  
-- | Lookup a variable in the type environment  
lookupVarType :: Id -> TypeEnv -> Poly
lookupVarType x ((y, s) : gamma)
  | x == y    = s
  | otherwise = lookupVarType x gamma
lookupVarType x [] = throw (Error ("unbound variable: " ++ x))

-- | Extend the type environment with a new biding
extendTypeEnv :: Id -> Poly -> TypeEnv -> TypeEnv
extendTypeEnv x s gamma = (x,s) : gamma

-- | Lookup a type variable in a substitution;
--   if not present, return the variable unchanged
lookupTVar :: TVar -> Subst -> Type
lookupTVar id []             = (TVar id)
lookupTVar id ((key, value):env)
           | id == key       = value
           | otherwise       = lookupTVar id env

-- | Remove a type variable from a substitution
removeTVar :: TVar -> Subst -> Subst
removeTVar a sub = L.filter (\(x, _) -> x /= a) sub
     
-- | Things to which type substitutions can be apply
class Substitutable a where
  apply :: Subst -> a -> a
  
-- | Apply substitution to type
instance Substitutable Type where  
  apply sub TInt         = TInt
  apply sub TBool        = TBool
  apply sub (TVar a)     = lookupTVar a sub
  apply sub (x :=> y)    = apply sub x :=> apply sub y
  apply sub (TList t)    = TList (apply sub t)

-- | Apply substitution to poly-type
instance Substitutable Poly where
  apply sub (Mono t)          = Mono (apply sub t)
  apply sub (Forall x t)      = Forall x $ apply (removeTVar x sub) t 

-- | Apply substitution to (all poly-types in) another substitution
instance Substitutable Subst where  
  apply sub to = zip keys $ map (apply sub) vals
    where
      (keys, vals) = unzip to
      
-- | Apply substitution to a type environment
instance Substitutable TypeEnv where  
  apply sub gamma = zip keys $ map (apply sub) vals
    where
      (keys, vals) = unzip gamma
      
-- | Extend substitution with a new type assignment
extendSubst :: Subst -> TVar -> Type -> Subst
extendSubst sub a t = (a, t) : (L.map (\(x, y) -> (x, apply [(a, t)] y)) sub)


--------------------------------------------------------------------------------
-- Problem 2: Unification
--------------------------------------------------------------------------------
      
-- | State of the type inference algorithm      
data InferState = InferState { 
    stSub :: Subst -- ^ current substitution
  , stCnt :: Int   -- ^ number of fresh type variables generated so far
} deriving Show

-- | Initial state: empty substitution; 0 type variables
initInferState = InferState [] 0

-- | Fresh type variable number n
freshTV n = TVar $ "a" ++ show n      
    
-- | Extend the current substitution of a state with a new type assignment   
extendState :: InferState -> TVar -> Type -> InferState
extendState (InferState sub n) a t = InferState (extendSubst sub a t) n
        
-- | Unify a type variable with a type; 
--   if successful return an updated state, otherwise throw an error
-- 1. if TVar == Type return InferState
-- 2. if the free variables in the type contain TVar, throw error
-- 3. otherwise extend the current sub with new type assignment (the extendState function provided) 
unifyTVar :: InferState -> TVar -> Type -> InferState
unifyTVar st a t
          | TVar a == t               = st
          | L.elem a (freeTVars t)    = throw (Error ("type error: cannot unify " ++ a ++ " and " ++ show t ++ " (occurs check)"))
          | otherwise                 = extendState st a t
    
-- | Unify two types;
--   if successful return an updated state, otherwise throw an error
unify :: InferState -> Type -> Type -> InferState
unify st TInt       TInt                   = st
unify st TBool      TBool                  = st
unify st (TVar a)   t                      = unifyTVar st a t
unify st t          (TVar a)               = unifyTVar st a t
unify st (a :=> b)  (c :=> d)              = unify (unify st applyA applyC) applyB applyD -- (credit: https://nadia-polikarpova.github.io/cse130-web/static/raw/disc-pa5tips.pdf)
  where
    grabFromInfer = stSub st
    applyA        = apply grabFromInfer a
    applyC        = apply grabFromInfer c
    applyB        = apply grabFromInfer b
    applyD        = apply grabFromInfer d
unify st (TList t)  (TList u)              = unify st t u
unify st t1         t2                     = throw (Error ("type error: cannot unify " ++ show t1 ++ " and " ++ show t2))

--------------------------------------------------------------------------------
-- Problem 3: Type Inference
--------------------------------------------------------------------------------    
  
infer :: InferState -> TypeEnv -> Expr -> (InferState, Type)
infer st _   (EInt _)          = (st, TInt)
infer st _   (EBool _)         = (st, TBool)  
infer st gamma (EVar x)        = (st, helper (lookupVarType x gamma))
  where
    helper (Mono m)            = m
    -- helper (Forall f)
--infer st gamma (ELam x e)      = tX :=> tBody
--  where
--    tEnv' = extendTypeEnv x tX gamma
--    tX    = freshTV (stCnt st)
--    tBody = infer tEnv' e
infer st gamma (EApp e1 e2)    = error "TBD: infer EApp"
infer st gamma (ELet x e1 e2)  = error "TBD: infer ELet"
infer st gamma (EBin op e1 e2) = infer st gamma asApp
  where
    asApp = EApp (EApp opVar e1) e2
    opVar = EVar (show op)
infer st gamma (EIf c e1 e2) = infer st gamma asApp
  where
    asApp = EApp (EApp (EApp ifVar c) e1) e2
    ifVar = EVar "if"    
infer st gamma ENil = infer st gamma (EVar "[]")

generalize :: TypeEnv -> Type -> Poly
generalize gamma t
            | uvs == []            = Mono t
            | otherwise            = helper uvs
  where
    tvs    = freeTVars t
    gvs    = freeTVars gamma
    uvs    = L.nub (tvs L.\\ gvs)
    helper :: [TVar] -> Poly
    helper (x:xs) = Forall x (foldr Forall (Mono t) xs) -- Piazza @1190 @1166 @1125, UCSD PA5 tips slides (https://nadia-polikarpova.github.io/cse130-web/static/raw/disc-pa5tips.pdf)
    
-- | Instantiate a polymorphic type into a mono-type with fresh type variables
instantiate :: Int -> Poly -> (Int, Type)
instantiate n (Forall as t) = error "TBD: instantiate" --(su', apply suInst t)
--  where
--    (su', as')        = freshTV n m
--    suInst            = L.zip as as'
--    m                 = length as
      
-- | Types of built-in operators and functions      
preludeTypes :: TypeEnv
preludeTypes =
  [ ("+",    Mono $ TInt :=> TInt :=> TInt)
  , ("-",    error "TBD: -")
  , ("*",    error "TBD: *")
  , ("/",    error "TBD: /")
  , ("==",   error "TBD: ==")
  , ("!=",   error "TBD: !=")
  , ("<",    error "TBD: <")
  , ("<=",   error "TBD: <=")
  , ("&&",   error "TBD: &&")
  , ("||",   error "TBD: ||")
  , ("if",   error "TBD: if")
  -- lists: 
  , ("[]",   error "TBD: []")
  , (":",    error "TBD: :")
  , ("head", error "TBD: head")
  , ("tail", error "TBD: tail")
  ]
