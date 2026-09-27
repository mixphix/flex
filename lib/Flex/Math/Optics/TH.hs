module Flex.Math.Optics.TH where

import Flex.Math.Numbers

import Control.Applicative qualified as Control
import Data.Char (Char)
import Data.Eq ((==))
import Data.Foldable qualified as Data
import Data.Function ((.))
import Data.Functor qualified as Data
import Data.List (replicate)
import Data.Maybe
import Data.Semigroup ((<>))
import Language.Haskell.TH
import Text.Show (show)

-- |
-- @'fieldN' 0@ generates the declaration:
--
-- > class Field0 xs ys x y
-- >   | xs -> x, ys -> y, xs y -> ys, ys x -> xs
-- >   where
-- >   _0 :: Lens xs ys x y
fieldN :: Natural -> Q Dec
fieldN n =
  classD
    (Control.pure [])
    (mkName ("Field" <> show n))
    [ PlainTV (mkName "xs") BndrReq
    , PlainTV (mkName "ys") BndrReq
    , PlainTV (mkName "x") BndrReq
    , PlainTV (mkName "y") BndrReq
    ]
    [ FunDep [mkName "xs"] [mkName "x"]
    , FunDep [mkName "ys"] [mkName "y"]
    , FunDep [mkName "xs", mkName "y"] [mkName "ys"]
    , FunDep [mkName "ys", mkName "x"] [mkName "xs"]
    ]
    [ sigD (mkName ("_" <> show n)) do
        appT
          ( appT
              ( appT
                  ( appT
                      (conT (mkName "Lens"))
                      (varT (mkName "xs"))
                  )
                  (varT (mkName "ys"))
              )
              (varT (mkName "x"))
          )
          (varT (mkName "y"))
    ]

-- |
-- @'generate' 2 "x" f@ generates the list @[f 0 "x", f 1 "x", f 2 "x"]@.
generate :: Natural -> [Char] -> (Natural -> [Char] -> Q Type) -> [Q Type]
generate n prefix n_p_qt = Data.fmap (`n_p_qt` prefix) [0 .. n]

-- |
-- @'instanceField' 2 1@ generates the declaration:
--
-- > instance Field2 (x0, x1, x2) (x0, x1, x2') x2 x2' where
-- >   _2 k (x0, x1, x2) = morphism (\x2' -> (x0, x1, x2')) (k x2)
instanceField :: Natural -> Natural -> Q Dec
instanceField m n =
  instanceD
    (Control.pure [])
    ( appT
        ( appT
            ( appT
                ( appT
                    (conT (mkName ("Field" <> show m)))
                    ( Data.foldl'
                        appT
                        (tupleT (from (n + 1)))
                        (generate n "x" \k pfx -> varT (mkName (pfx <> show k)))
                    )
                )
                ( Data.foldl'
                    appT
                    (tupleT (from (n + 1)))
                    ( generate n "x" \k pfx -> varT (mkName (pfx <> show k <> if k == m then "'" else ""))
                    )
                )
            )
            (varT (mkName ("x" <> show m)))
        )
        (varT (mkName ("x" <> show m <> "'")))
    )
    [ funD
        (mkName ("_" <> show m))
        [ clause
            [varP (mkName "k"), tupP (Data.fmap (varP . mkName . ("x" <>) . show) [0 .. n])]
            ( normalB
                ( appE
                    ( appE
                        (varE (mkName "morphism"))
                        ( lamE
                            [varP (mkName ("x" <> show m <> "'"))]
                            ( tupE
                                ( Data.fmap
                                    (\i -> varE (mkName ("x" <> show i <> if i == m then "'" else "")))
                                    [0 .. n]
                                )
                            )
                        )
                    )
                    ( appE
                        (varE (mkName "k"))
                        (varE (mkName ("x" <> show m)))
                    )
                )
            )
            []
        ]
    ]

-- |
-- @'instanceFieldV' 0 3@ generates the declaration:
--
-- > instance Field0 (V 3 x) (V 3 x) x x where
-- >   _0 k v = morphism (\x' -> setV 0 x' v) (k (v ! 0))
instanceFieldV :: Natural -> Natural -> Q Dec
instanceFieldV m n =
  instanceD
    (Control.pure [])
    ( appT
        ( appT
            ( appT
                ( appT
                    (conT (mkName ("Field" <> show m)))
                    ( appT
                        (appT (conT (mkName "V")) (litT (Control.pure (NumTyLit (from n)))))
                        (varT (mkName "x"))
                    )
                )
                ( appT
                    (appT (conT (mkName "V")) (litT (Control.pure (NumTyLit (from n)))))
                    (varT (mkName "x"))
                )
            )
            (varT (mkName "x"))
        )
        (varT (mkName "x"))
    )
    [ funD
        (mkName ("_" <> show m))
        [ clause
            [varP (mkName "k"), varP (mkName "v")]
            ( normalB
                ( appE
                    ( appE
                        (varE (mkName "morphism"))
                        ( lamE
                            [varP (mkName "x'")]
                            ( appE
                                ( appE
                                    ( appE
                                        (varE (mkName "setV"))
                                        (litE (integerL (from m)))
                                    )
                                    (varE (mkName "x'"))
                                )
                                (varE (mkName "v"))
                            )
                        )
                    )
                    ( appE
                        (varE (mkName "k"))
                        ( infixE
                            (Just (varE (mkName "v")))
                            (varE (mkName "!"))
                            (Just (litE (IntegerL (from m))))
                        )
                    )
                )
            )
            []
        ]
    ]

-- |
-- @'instanceEach' 1@ generates the declaration:
--
-- > instance Each (x, x) (x', x') x x' where
-- >   each :: Traversal (x, x) (x', x') x x'
-- >   each k (x0, x1) = pure (,) <*> k x0 <*> k x1
instanceEach :: Natural -> Q Dec
instanceEach n =
  instanceD
    (Control.pure [])
    ( appT
        ( appT
            ( appT
                ( appT
                    (conT (mkName "Each"))
                    ( Data.foldl'
                        appT
                        (tupleT (from (n + 1)))
                        (generate n "x" \_ pfx -> varT (mkName pfx))
                    )
                )
                ( Data.foldl'
                    appT
                    (tupleT (from (n + 1)))
                    (generate n "x'" \_ pfx -> varT (mkName pfx))
                )
            )
            (varT (mkName "x"))
        )
        (varT (mkName "x'"))
    )
    [ funD
        (mkName "each")
        [ clause
            [varP (mkName "k"), tupP (Data.fmap (varP . mkName . ("x" <>) . show) [0 .. n])]
            ( normalB
                ( Data.foldl'
                    (\x y -> infixE (Just x) (varE (mkName "<*>")) (Just y))
                    ( appE
                        (varE (mkName "pure"))
                        (Control.pure (TupE (replicate (from (n + 1)) Nothing)))
                    )
                    (Data.fmap (\i -> appE (varE (mkName "k")) (varE (mkName ("x" <> show i)))) [0 .. n])
                )
            )
            []
        ]
    ]
