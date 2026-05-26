import Data.List
import Decidable.Equality
import Control.WellFounded

import Data.Nat

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

