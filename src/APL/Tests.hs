module APL.Tests
  ( properties,
  )
where

import APL.AST (Exp (..), VName, printExp, subExp)
import APL.Check (checkExp)
import APL.Error (isDomainError, isTypeError, isVariableError)
import APL.Eval (eval, runEval)
import APL.Parser (keywords, parseAPL)
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink),
    Gen,
    Property,
    checkCoverage,
    chooseInt,
    cover,
    elements,
    frequency,
    oneof,
    property,
    sized,
    vectorOf,
  )

instance Arbitrary Exp where
  arbitrary = sized $ genExp []

  shrink (Add e1 e2) =
    e1 : e2 : [Add e1' e2 | e1' <- shrink e1] ++ [Add e1 e2' | e2' <- shrink e2]
  shrink (Sub e1 e2) =
    e1 : e2 : [Sub e1' e2 | e1' <- shrink e1] ++ [Sub e1 e2' | e2' <- shrink e2]
  shrink (Mul e1 e2) =
    e1 : e2 : [Mul e1' e2 | e1' <- shrink e1] ++ [Mul e1 e2' | e2' <- shrink e2]
  shrink (Div e1 e2) =
    e1 : e2 : [Div e1' e2 | e1' <- shrink e1] ++ [Div e1 e2' | e2' <- shrink e2]
  shrink (Pow e1 e2) =
    e1 : e2 : [Pow e1' e2 | e1' <- shrink e1] ++ [Pow e1 e2' | e2' <- shrink e2]
  shrink (Eql e1 e2) =
    e1 : e2 : [Eql e1' e2 | e1' <- shrink e1] ++ [Eql e1 e2' | e2' <- shrink e2]
  shrink (If cond e1 e2) =
    e1 : e2 : [If cond' e1 e2 | cond' <- shrink cond] ++ [If cond e1' e2 | e1' <- shrink e1] ++ [If cond e1 e2' | e2' <- shrink e2]
  shrink (Let x e1 e2) =
    e1 : [Let x e1' e2 | e1' <- shrink e1] ++ [Let x e1 e2' | e2' <- shrink e2]
  shrink (Lambda x e) =
    [Lambda x e' | e' <- shrink e]
  shrink (Apply e1 e2) =
    e1 : e2 : [Apply e1' e2 | e1' <- shrink e1] ++ [Apply e1 e2' | e2' <- shrink e2]
  shrink (TryCatch e1 e2) =
    e1 : e2 : [TryCatch e1' e2 | e1' <- shrink e1] ++ [TryCatch e1 e2' | e2' <- shrink e2]
  shrink _ = []

genExp :: [VName] -> Int -> Gen Exp
genExp _ 0 = oneof [CstInt <$> arbitrary, CstBool <$> arbitrary]
genExp v size =
  frequency
    [ (12, CstInt <$> arbitrary),
      (12, CstBool <$> arbitrary),
      (12, Add <$> genExp v halfSize <*> genExp v halfSize),
      (12, Sub <$> genExp v halfSize <*> genExp v halfSize),
      (12, Mul <$> genExp v halfSize <*> genExp v halfSize),
      (5, Div <$> genExp v halfSize <*> genExp v halfSize),
      (5, Pow <$> genExp v halfSize <*> genExp v halfSize),
      (10, Eql <$> genExp v halfSize <*> genExp v halfSize),
      (10, If <$> genExp v thirdSize <*> genExp v thirdSize <*> genExp v thirdSize),
      (5, genVar v),
      (10, genLet v),
      (10, genLambda v),
      (20, Apply <$> genExp v halfSize <*> genExp v halfSize),
      (20, TryCatch <$> genExp v halfSize <*> genExp v halfSize)
    ]
  where
    halfSize = size `div` 2
    thirdSize = size `div` 3

    genVarName = do
      n <-
        frequency
          [ (200, genShortVar),
            (5, genLongVar)
          ]
      if n `elem` keywords then genVarName else pure n
      where
        genShortVar = do
          len <- chooseInt (2, 4)
          first <- elements ['a' .. 'z']
          rest <- vectorOf (len - 1) (elements ['a' .. 'z'])
          pure (first : rest)
        genLongVar = do
          len <- chooseInt (5, 10)
          first <- elements ['a' .. 'z']
          rest <- vectorOf (len - 1) (elements ['a' .. 'z'])
          pure (first : rest)

    genVar [] =
      Var
        <$> genVarName
    genVar vs =
      frequency
        [ (10, Var <$> elements vs),
          (1, Var <$> genVarName)
        ]

    genLet vs = do
      x <- genVarName
      e1 <- genExp vs halfSize
      e2 <- genExp (x : vs) halfSize
      pure $ Let x e1 e2

    genLambda vs = do
      x <- genVarName
      e <- genExp (x : vs) (size - 1)
      pure $ Lambda x e

expCoverage :: Exp -> Property
expCoverage e =
  checkCoverage
    . cover 20 (any isDomainError (checkExp e)) "domain error"
    . cover 20 (not $ any isDomainError (checkExp e)) "no domain error"
    . cover 20 (any isTypeError (checkExp e)) "type error"
    . cover 20 (not $ any isTypeError (checkExp e)) "no type error"
    . cover 5 (any isVariableError (checkExp e)) "variable error"
    . cover 70 (not $ any isVariableError (checkExp e)) "no variable error"
    . cover 50 (or [2 <= n && n <= 4 | Var v <- subExp e, let n = length v]) "non-trivial variable"
    $ ()

parsePrinted :: Exp -> Bool
parsePrinted e =
  case parseAPL "" (printExp e) of
    Left _ -> False
    Right res -> res == e

onlyCheckedErrors :: Exp -> Bool
onlyCheckedErrors e =
  case runEval $ eval e of
    Left err -> err `elem` checkExp e
    Right _ -> True

properties :: [(String, Property)]
properties =
  [ ("expCoverage", property expCoverage),
    ("onlyCheckedErrors", property onlyCheckedErrors),
    ("parsePrinted", property parsePrinted)
  ]
