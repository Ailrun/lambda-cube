{-# LANGUAGE QuasiQuotes #-}
module LambdaCube.SystemFwTestExample where

import           LambdaCube.SystemFw

lcBaseId, lcBaseArrId :: ExtLCTerm
lcBaseId = [qTerm| \x : # . x |]
lcBaseArrId = [qTerm| \x : $lcBaseArr . x |]

lcBaseArr :: ExtLCType
lcBaseArr = [qType| # -> # |]

lcId :: ExtLCTerm
lcId = [qTerm| @\a : * . \x : a . x |]

------------------------------------------------------------
-- Church Pair examples
------------------------------------------------------------

lcCPPair :: ExtLCTerm
lcCPPair = [qTerm| @\a : * . @\b : * . \x : a . \y : b . @\r : * . \f : a -> b -> r . f x y |]

lcCPFst :: ExtLCTerm
lcCPFst = [qTerm| @\a : * . @\b : * . \p : $lcCPTy a b . p @a (\x : a . \y : b . x) |]

lcCPSnd :: ExtLCTerm
lcCPSnd = [qTerm| @\a : * . @\b : * . \p : $lcCPTy a b . p @b (\x : a . \y : b . y) |]

lcCPTy :: ExtLCType
lcCPTy = [qType| \a : * . \b : * . !r : * , (a -> b -> r) -> r |]

------------------------------------------------------------
-- Church Boolean examples
------------------------------------------------------------

lcCBTrue, lcCBFalse :: ExtLCTerm
lcCBTrue = [qTerm| @\r : * . \t : r . \f : r . t |]
lcCBFalse = [qTerm| @\r : * . \t : r . \f : r . f |]

lcCBIf :: ExtLCTerm
lcCBIf = [qTerm| \b : $lcCBTy . b |]

lcCBAnd, lcCBOr :: ExtLCTerm
lcCBAnd = [qTerm| \b1 : $lcCBTy . \b2 : $lcCBTy . @\r : * . \t : r . \f : r . b1 @r (b2 @r t f) f |]
lcCBOr = [qTerm| \b1 : $lcCBTy . \b2 : $lcCBTy . @\r : * . \t : r . \f : r . b1 @r t (b2 @r t f) |]

lcCBNot :: ExtLCTerm
lcCBNot = [qTerm| \b : $lcCBTy . @\r : * . \t : r . \f : r . b @r f t |]

lcCBTy :: ExtLCType
lcCBTy = [qType| !r : * , r -> r -> r |]

------------------------------------------------------------
-- Church Numeral examples
------------------------------------------------------------

lcCN1, lcCN2, lcCN3, lcCN4, lcCN5 :: ExtLCTerm
lcCN1 = [qTerm| @\r : * . \s : r -> r . \z : r . s z |]
lcCN2 = [qTerm| @\r : * . \s : r -> r . \z : r . s (s z) |]
lcCN3 = [qTerm| @\r : * . \s : r -> r . \z : r . s (s (s z)) |]
lcCN4 = [qTerm| @\r : * . \s : r -> r . \z : r . s (s (s (s z))) |]
lcCN5 = [qTerm| @\r : * . \s : r -> r . \z : r . s (s (s (s (s z)))) |]

lcCN0, lcCNSucc :: ExtLCTerm
lcCN0 = [qTerm| @\r : * . \s : r -> r . \z : r . z |]
lcCNSucc = [qTerm| \n : $lcCNTy . @\r : * . \s : r -> r . \z : r . s (n @r s z) |]

lcCNPred, lcCNPredDef :: ExtLCTerm
lcCNPred =
  [qTerm|
     \n : $lcCNTy .
     @\r : * . \s : r -> r . \z : r .
     n
     @((r -> r) -> r)
     (\p : (r -> r) -> r . \g : r -> r . g (p s))
     (\p : r -> r . z)
     (\p : r . p)
  |]
lcCNPredDef =
  [qTerm|
     \d : $lcCNTy .
     \n : $lcCNTy .
     n
     @(($lcCNTy -> $lcCNTy -> $lcCNTy) -> $lcCNTy)
     (\p : ($lcCNTy -> $lcCNTy -> $lcCNTy) -> $lcCNTy .
      \g : $lcCNTy -> $lcCNTy -> $lcCNTy .
      (\x : $lcCNTy . g ($lcCNSucc x) x) (p (\x : $lcCNTy . \y : $lcCNTy . x)))
     (\g : $lcCNTy -> $lcCNTy -> $lcCNTy . g $lcCN0 d)
     (\x : $lcCNTy . \y : $lcCNTy . y)
  |]

lcCNAdd, lcCNMul, lcCNMinus :: ExtLCTerm
lcCNAdd =
  [qTerm|
     \n : $lcCNTy . \m : $lcCNTy .
     @\r : * . \s : r -> r . \z : r . n @r s (m @r s z)
  |]
lcCNMul =
  [qTerm|
     \n : $lcCNTy . \m : $lcCNTy .
     @\r : * . \s : r -> r . \z : r . n @r (m @r s) z
  |]
lcCNMinus =
  [qTerm|
     \n : $lcCNTy . \m : $lcCNTy . m @$lcCNTy $lcCNPred n
  |]

lcCNIs0 :: ExtLCTerm
lcCNIs0 =
  [qTerm|
     \n : $lcCNTy . n @$lcCBTy (\x : $lcCBTy . $lcCBFalse) $lcCBTrue
  |]

lcCNTy :: ExtLCType
lcCNTy = [qType| !r : * , (r -> r) -> r -> r |]

------------------------------------------------------------
-- Church List examples
------------------------------------------------------------

lcCL_CN_0, lcCL_CN_0_1, lcCL_CN_0_1_2 :: ExtLCTerm
lcCL_CN_0 =
  [qTerm|
    $lcCLCons @$lcCNTy $lcCN0
    ($lcCLNil @$lcCNTy)
  |]
lcCL_CN_0_1 =
  [qTerm|
    $lcCLCons @$lcCNTy $lcCN0
    ($lcCLCons @$lcCNTy $lcCN1
    ($lcCLNil @$lcCNTy))
  |]
lcCL_CN_0_1_2 =
  [qTerm|
    $lcCLCons @$lcCNTy $lcCN0
    ($lcCLCons @$lcCNTy $lcCN1
    ($lcCLCons @$lcCNTy $lcCN2
    ($lcCLNil @$lcCNTy)))
  |]

lcCLNil, lcCLCons :: ExtLCTerm
lcCLNil = [qTerm| @\e : * . @\r : * . \c : e -> r -> r . \n : r . n |]
lcCLCons =
  [qTerm|
    @\e : * .
    \h : e . \t : $lcCLTy e .
    @\r : * . \c : e -> r -> r . \n : r . c h (t @r c n)
  |]

lcCLLength :: ExtLCTerm
lcCLLength = [qTerm| @\e : * . \l : $lcCLTy e . l @$lcCNTy (\h : e . \t : $lcCNTy . $lcCNSucc t) $lcCN0 |]

lcCLHeadDef :: ExtLCTerm
lcCLHeadDef = [qTerm| @\e : * . \d : e . \l : $lcCLTy e . l @e (\h : e . \t : e . h) d |]

lcCLTail, lcCLTailDef :: ExtLCTerm
lcCLTail =
  [qTerm|
    @\e : * .
    \l : $lcCLTy e .
    @\r : * . \c : e -> r -> r . \n : r .
    l
    @((e -> r -> r) -> r)
    (\h : e . \t : (e -> r -> r) -> r . \g : e -> r -> r . g h (t c))
    (\t : e -> r -> r . n)
    (\h : e . \t : r . t)
  |]
lcCLTailDef =
  [qTerm|
    @\e : * .
    \d : $lcCLTy e .
    \l : $lcCLTy e .
    l
    @(($lcCLTy e -> $lcCLTy e -> $lcCLTy e) -> $lcCLTy e)
    (\h : e .
     \t : ($lcCLTy e -> $lcCLTy e -> $lcCLTy e) -> $lcCLTy e .
     \g : $lcCLTy e -> $lcCLTy e -> $lcCLTy e .
     (\x : $lcCLTy e . g ($lcCLCons @e h x) x) (t (\x : $lcCLTy e . \y : $lcCLTy e . x)))
    (\g : $lcCLTy e -> $lcCLTy e -> $lcCLTy e . g ($lcCLNil @e) d)
    (\x : $lcCLTy e . \y : $lcCLTy e . y)
  |]

lcCLTy :: ExtLCType
lcCLTy = [qType| \e : * . !r : * , (e -> r -> r) -> r -> r |]
