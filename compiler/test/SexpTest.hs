module SexpTest where

import Imports
import Sexp qualified as S
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Sexp"
    [ ( testCase "first" do
          let s = "(first (second third))".t
          let tokens = S.tokenize s
          -- tokens @?= [S.LParen, S.Thing "first".t, S.LParen, S.Thing "second".t, S.Thing "third".t, S.RParen, S.RParen]
          let res = (fmap . fmap) (() <$) $ S.parse s
          res @?= Right [S.List [S.Atom "first".t, S.List [S.Atom "second".t, S.Atom "third".t]]]
          pure ()
      ),
      ( testCase "block" do
          let s = "(label first (let (y i32) (add (x i32) (y i32))))".t
          let ts = S.tokenize s
          let res = (fmap . fmap) (() <$) $ S.parse s
          res
            @?= Right
              [ S.List
                  [ S.Atom "label".t,
                    S.Atom "first".t,
                    S.List
                      [ S.Atom "let".t,
                        S.List [S.Atom "y".t, S.Atom "i32".t],
                        S.List
                          [ S.Atom "add".t,
                            S.List [S.Atom "x".t, S.Atom "i32".t],
                            S.List [S.Atom "y".t, S.Atom "i32".t]
                          ]
                      ]
                  ]
              ]
      )
    ]
