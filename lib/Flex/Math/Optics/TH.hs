module Flex.Math.Optics.TH where

import Flex.Math.Category
import Flex.Math.Numbers

import Control.Applicative qualified as Control
import Data.Char (Char)
import Data.Eq (Eq (..), (==))
import Data.Foldable qualified as Data
import Data.Functor qualified as Data
import Data.List (replicate)
import Data.Maybe
import Language.Haskell.TH
import Text.Show (show)

-- |
-- @'fieldN' 0@ generates the declaration:
--
-- > class Field0 xs ys x y
-- >   | xs -> x, ys -> y, xs y -> ys, ys x -> xs
-- >   where
-- >   _0 :: Lens xs ys x y
fieldN :: Int -> Q Dec
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
generate :: Int -> [Char] -> (Int -> [Char] -> Q Type) -> [Q Type]
generate n prefix n_p_qt = Data.fmap (`n_p_qt` prefix) [0 .. n]

-- |
-- @'instanceField' 2 1@ generates the declaration:
--
-- > instance Field2 (x0, x1, x2) (x0, x1, x2') x2 x2' where
-- >   _2 k (x0, x1, x2) = morphism (\x2' -> (x0, x1, x2')) (k x2)
instanceField :: Int -> Int -> Q Dec
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
                        (tupleT (n + 1))
                        (generate n "x" \k pfx -> varT (mkName (pfx <> show k)))
                    )
                )
                ( Data.foldl'
                    appT
                    (tupleT (n + 1))
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
instanceFieldV :: Int -> Int -> Q Dec
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

instanceIndices :: Int -> Q Dec
instanceIndices n = do
  let patternMatch q =
        match
          (litP (integerL (from q)))
          ( normalB
              ( appE
                  ( appE
                      (varE (mkName "morphism"))
                      ( case n of
                          0 -> conE (mkName "MkSolo")
                          _ ->
                            Control.pure
                              ( TupE
                                  ( Data.fmap
                                      ( \i -> do
                                          guard (i /= q)
                                          Just (VarE (mkName ("x" <> show i)))
                                      )
                                      [0 .. n]
                                  )
                              )
                      )
                  )
                  (appE (varE (mkName "x_fx")) (varE (mkName ("x" <> show q))))
              )
          )
          []

  instanceD
    (Control.pure [])
    ( appT
        (conT (mkName "Indices"))
        ( Data.foldl'
            appT
            (tupleT (n + 1))
            (generate n "x" \_ pfx -> varT (mkName pfx))
        )
    )
    [ tySynInstD
        ( tySynEqn
            Nothing
            ( appT
                (conT (mkName "Index"))
                ( Data.foldl'
                    appT
                    (tupleT (n + 1))
                    (generate n "x" \_ pfx -> varT (mkName pfx))
                )
            )
            (appT (conT (mkName "Finite")) (litT (Control.pure (NumTyLit (from n + 1)))))
        )
    , tySynInstD
        ( tySynEqn
            Nothing
            ( appT
                (conT (mkName "Value"))
                ( Data.foldl'
                    appT
                    (tupleT (n + 1))
                    (generate n "x" \_ pfx -> varT (mkName pfx))
                )
            )
            (varT (mkName "x"))
        )
    , funD
        (mkName "index")
        [ clause
            [ varP (mkName "i")
            , varP (mkName "x_fx")
            , tupP (Data.fmap (varP . mkName . ("x" <>) . show) [0 .. n])
            ]
            ( normalB
                ( caseE
                    (appE (varE (mkName "getFinite")) (varE (mkName "i")))
                    ( Data.fmap patternMatch [0 .. n]
                        <> [ match
                               wildP
                               ( normalB
                                   ( appE
                                       ( {- appE -}
                                         (varE (mkName "pure"))
                                         {- (appE (varE (mkName "morphism")) (varE (mkName "x_fx"))) -}
                                       )
                                       (tupE (Data.fmap (varE . mkName . ("x" <>) . show) [0 .. n]))
                                   )
                               )
                               []
                           ]
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
instanceEach :: Int -> Q Dec
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
                        (tupleT (n + 1))
                        (generate n "x" \_ pfx -> varT (mkName pfx))
                    )
                )
                ( Data.foldl'
                    appT
                    (tupleT (n + 1))
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
                        (Control.pure (TupE (replicate (n + 1) Nothing)))
                    )
                    ( Data.fmap
                        (\i -> appE (varE (mkName "k")) (varE (mkName ("x" <> show i))))
                        [0 .. n]
                    )
                )
            )
            []
        ]
    ]

-- |
-- @'instanceIxEach' 1@ generates the declaration:
--
-- > instance IxEach (Finite 2) (x, x) (x', x') x x' where
-- >   ixeach :: IxTraversal (Finite 2) (x, x) (x', x') x x'
-- >   ixeach k (x0, x1) = pure (,) <*> ixed k 0 x0 <*> ixed k 1 x1
instanceIxEach :: Int -> Q Dec
instanceIxEach n =
  instanceD
    (Control.pure [])
    ( appT
        ( appT
            ( appT
                ( appT
                    ( appT
                        (conT (mkName "IxEach"))
                        (appT (conT (mkName "Finite")) (litT (Control.pure (NumTyLit (from n + 1)))))
                    )
                    ( Data.foldl'
                        appT
                        (tupleT (n + 1))
                        (generate n "x" \_ pfx -> varT (mkName pfx))
                    )
                )
                ( Data.foldl'
                    appT
                    (tupleT (n + 1))
                    (generate n "x'" \_ pfx -> varT (mkName pfx))
                )
            )
            (varT (mkName "x"))
        )
        (varT (mkName "x'"))
    )
    [ funD
        (mkName "ixeach")
        [ clause
            [varP (mkName "k"), tupP (Data.fmap (varP . mkName . ("x" <>) . show) [0 .. n])]
            ( normalB
                ( Data.foldl'
                    (\x y -> infixE (Just x) (varE (mkName "<*>")) (Just y))
                    ( appE
                        (varE (mkName "pure"))
                        ( case n of
                            0 -> conE (mkName "MkSolo")
                            _ -> Control.pure (TupE (replicate (n + 1) Nothing))
                        )
                    )
                    ( Data.fmap
                        ( \i ->
                            appE
                              ( appE
                                  ( appE
                                      (varE (mkName "ixed"))
                                      (varE (mkName "k"))
                                  )
                                  ( sigE
                                      ( appE
                                          (varE (mkName "from"))
                                          (sigE (litE (integerL (from i))) (conT (mkName "Integer")))
                                      )
                                      (appT (conT (mkName "Finite")) (litT (Control.pure (NumTyLit (from n + 1)))))
                                  )
                              )
                              (varE (mkName ("x" <> show i)))
                        )
                        [0 .. n]
                    )
                )
            )
            []
        ]
    ]
