module Main

%default total

data Term = TmTrue
          | TmFalse
          | TmIf Term Term Term
          | TmZero
          | TmSucc Term
          | TmPred Term
          | TmIsZero Term

isNumericVal : Term -> Bool
isNumericVal TmZero = True
isNumericVal (TmSucc t) = isNumericVal t
isNumericVal _ = False

isVal : Term -> Bool
isVal TmTrue = True
isVal TmFalse = True
isVal t = isNumericVal t

eval1 : Term -> Maybe Term
eval1 (TmIf TmTrue t2 t3) = Just t2
eval1 (TmIf TmFalse t2 t3) = Just t3
eval1 (TmIf t1 t2 t3) =
    case eval1 t1 of
         Just t1' => Just (TmIf t1' t2 t3)
         Nothing => Nothing
eval1 (TmSucc t1) =
    case eval1 t1 of
         Just t1' => Just (TmSucc t1')
         Nothing => Nothing
eval1 (TmPred TmZero) = Just TmZero
eval1 (TmPred (TmSucc nv1)) =
    if isNumericVal nv1 then Just nv1 else Nothing
eval1 (TmPred t1) =
    case eval1 t1 of
         Just t1' => Just (TmPred t1')
         Nothing => Nothing
eval1 (TmIsZero TmZero) = Just TmTrue
eval1 (TmIsZero (TmSucc nv1)) =
    if isNumericVal nv1 then Just TmFalse else Nothing
eval1 (TmIsZero t1) =
    case eval1 t1 of
         Just t1' => Just (TmIsZero t1')
         Nothing => Nothing
eval1 _ = Nothing
