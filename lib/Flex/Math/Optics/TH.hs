module Flex.Math.Optics.TH where

import Flex.Math.Numbers

import Control.Applicative qualified as Control
import Data.Char (Char)
import Data.Eq ((==))
import Data.Foldable qualified as Data
import Data.Function ((.))
import Data.Functor (Functor (fmap))
import Data.List (replicate)
import Data.Maybe
import Data.Semigroup ((<>))
import Language.Haskell.TH
import Text.Show (show)

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

generate :: [Char] -> Natural -> (Natural -> [Char] -> Q Type) -> [Q Type]
generate prefix n n_p_qt = fmap (`n_p_qt` prefix) [0 .. n]

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
                        (generate "x" n \k pfx -> varT (mkName (pfx <> show k)))
                    )
                )
                ( Data.foldl'
                    appT
                    (tupleT (from (n + 1)))
                    ( generate "x" n \k pfx -> varT (mkName (pfx <> show k <> if k == m then "'" else ""))
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
            [varP (mkName "k"), tupP (fmap (varP . mkName . ("x" <>) . show) [0 .. n])]
            ( normalB
                ( appE
                    ( appE
                        (varE (mkName "morphism"))
                        ( lamE
                            [varP (mkName ("x" <> show m <> "'"))]
                            ( tupE
                                ( fmap
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
                        (generate "x" n \_ pfx -> varT (mkName pfx))
                    )
                )
                ( Data.foldl'
                    appT
                    (tupleT (from (n + 1)))
                    (generate "x'" n \_ pfx -> varT (mkName pfx))
                )
            )
            (varT (mkName "x"))
        )
        (varT (mkName "x'"))
    )
    [ funD
        (mkName "each")
        [ clause
            [varP (mkName "k"), tupP (fmap (varP . mkName . ("x" <>) . show) [0 .. n])]
            ( normalB
                ( Data.foldl'
                    (\x y -> infixE (Just x) (varE (mkName "<*>")) (Just y))
                    ( appE
                        (varE (mkName "pure"))
                        (Control.pure (TupE (replicate (from (n + 1)) Nothing)))
                    )
                    (fmap (\i -> appE (varE (mkName "k")) (varE (mkName ("x" <> show i)))) [0 .. n])
                )
            )
            []
        ]
    ]
