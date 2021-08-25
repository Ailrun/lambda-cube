{-# LANGUAGE QuasiQuotes #-}
module LambdaCube.STLCTestExample where

import           LambdaCube.STLC

lcBaseId, lcBaseArrId :: ExtLCTerm
lcBaseId = [qTerm| \x : # . x |]
lcBaseArrId = [qTerm| \x : $lcBaseArr . x |]

lcBaseArr :: ExtLCType
lcBaseArr = [qType| # -> # |]

------------------------------------------------------------
-- Pseudo Church Numeral examples
------------------------------------------------------------

lcPCN0, lcPCN1, lcPCN2, lcPCN3, lcPCN4, lcPCN5 :: ExtLCTerm
lcPCN0 = [qTerm| \s : $lcBaseArr . $lcBaseId |]
lcPCN1 = [qTerm| \s : $lcBaseArr . \z : # . s z |]
lcPCN2 = [qTerm| \s : $lcBaseArr . \z : # . s (s z) |]
lcPCN3 = [qTerm| \s : $lcBaseArr . \z : # . s (s (s z)) |]
lcPCN4 = [qTerm| \s : $lcBaseArr . \z : # . s (s (s (s z))) |]
lcPCN5 = [qTerm| \s : $lcBaseArr . \z : # . s (s (s (s (s z)))) |]

lcPCNAdd, lcPCNMul :: ExtLCTerm
lcPCNAdd =
  [qTerm|
     \n : $lcPCNTy . \m : $lcPCNTy .
     \s : $lcBaseArr . \z : # . n s (m s z)
  |]
lcPCNMul =
  [qTerm|
     \n : $lcPCNTy . \m : $lcPCNTy .
     \s : $lcBaseArr . \z : # . n (m s) z
  |]

lcPCNTy :: ExtLCType
lcPCNTy = [qType| $lcBaseArr -> $lcBaseArr |]
