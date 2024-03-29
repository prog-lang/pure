module TypeChecker.StaticPrelude where

import TypeChecker.Static
  ( Assump ((:>:)),
    Kind (Kfun, Star),
    Pred (IsIn),
    Qual ((:=>)),
    Scheme (Forall),
    Tycon (Tycon),
    Type (TAp, TCon, TGen),
    addClass,
    addPreludeClasses,
    asig,
    atype,
    fn,
    instances,
    isIn1,
    mkInst,
    tChar,
    tDouble,
    tFloat,
    tInt,
    tInteger,
    tList,
    tString,
    tTuple2,
    tTuple3,
    tTuple4,
    tTuple5,
    tTuple6,
    tTuple7,
    tUnit,
    (<:>),
  )

-----------------------------------------------------------------------------
-- Standard Primitive Types:

-----------------------------------------------------------------------------
-- Definitions for the following primitive types are in TypeChecker.hs:
--   (), Char, Int, Integer, Float, Double, [], (->), and tuples size 2 to 7
--
-- Type assumptions for the constructors of these types are provided below:

consCfun =
  ":"
    :>: ( Forall
            [Star]
            ( []
                :=> ( TGen 0
                        `fn` TAp tList (TGen 0)
                        `fn` TAp tList (TGen 0)
                    )
            )
        )

tBool = TCon (Tycon "Bool" Star)

falseCfun = "False" :>: (Forall [] ([] :=> tBool))

trueCfun = "True" :>: (Forall [] ([] :=> tBool))

nilCfun = "[]" :>: (Forall [Star] ([] :=> (TAp tList (TGen 0))))

cMonad = "Monad"

mbindMfun =
  ">>="
    :>: ( Forall
            [Kfun Star Star, Star, Star]
            ( [isIn1 cMonad (TGen 0)]
                :=> (TAp (TGen 0) (TGen 1) `fn` (TGen 1 `fn` TAp (TGen 0) (TGen 2)) `fn` TAp (TGen 0) (TGen 2))
            )
        )
