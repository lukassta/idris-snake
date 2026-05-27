module Helper

import Data.Nat
import Data.Fin
import Data.List1
import Decidable.Equality
import System.Random

import Types
import Variables

%default covering

lte1IsSucc : LTE 1 a -> IsSucc a
lte1IsSucc (LTESucc _) = ItIsSucc


lteRefl : (n : Nat) -> LTE n n
lteRefl Z     = LTEZero
lteRefl (S n) = LTESucc (lteRefl n)


succMinus : (a, b : Nat) -> {auto bNZ: NonZero b} -> {auto bLTa: LT b a} -> (c: Nat ** (IsSucc c,  c `LT` a))
succMinus (S a) (S b) {bNZ} {bLTa = LTESucc bLTa} =
    case b of
        Z     =>
            let aSucc = lte1IsSucc bLTa in
            (a ** (aSucc, lteRefl (S a)))
        (S k) =>
            let (c ** (cSucc, scLTEa))= succMinus a (S k) in
            let scLTEsa = lteSuccRight scLTEa in
            (c ** (cSucc, scLTEsa))


lteTrans : LTE a b -> LTE b c -> LTE a c
lteTrans LTEZero _ = LTEZero
lteTrans (LTESucc x) (LTESucc y) = LTESucc $ lteTrans x y


public export
divWProof : (a, b : Nat) -> {auto aNZ: NonZero a} -> {auto bMTOne: LT 1 b} -> (c : Nat ** LT c a)
divWProof (S a) (S b) =
    case decEq (S a) (S b) of
        Yes saEQsb=>
            rewrite saEQsb in
            (1  **  bMTOne)
        No _ =>
            case b `isLT` a of
                Yes bLTa =>
                    let bSucc = lte1IsSucc (fromLteSucc bMTOne) in
                    let (a2 ** (a2Succ, sa2LTa)) = succMinus a b in
                    let (c ** scLTa2) = divWProof a2 (S b) in
                    let a2LTa = lteSuccLeft sa2LTa in
                    let scLTa = lteTrans scLTa2 a2LTa in
                    ((S c) ** (LTESucc scLTa))
                No bNLTa =>
                    case aNZ of
                        ItIsSucc => (0 ** (LTESucc LTEZero))


public export
randomElem : List a -> IO (Maybe a)
randomElem [] = pure Nothing
randomElem (head :: tail) = do
    let list = (head :: tail)
    randomId <- (rndFin  (length tail))
    pure $ Just $ randomElem' (head :: tail) randomId
where
    randomElem' : (list : List a) -> Fin (length list) -> a
    randomElem' (x :: xs)  FZ    = x
    randomElem' (x :: xs) (FS y) = randomElem' xs y


public export
removeMatcingElements : Eq a => (list1, list2 : List a) -> List a
removeMatcingElements []        _    = []
removeMatcingElements (x :: xs) list2 =
    case elem x list2 of
        True  => removeMatcingElements xs list2
        False => x :: removeMatcingElements xs list2


public export
populate : Coordinates -> List Coordinates
populate ( FZ   ,  FZ   ) = [( FZ   ,  FZ   )]
populate ( FZ   , (FS y)) =  ( FZ   , (FS y)) :: populate (last    , weaken y)
populate ((FS x), y     ) =  ((FS x), y     ) :: populate (weaken x, y       )
