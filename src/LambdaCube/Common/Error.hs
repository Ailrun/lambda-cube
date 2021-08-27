module LambdaCube.Common.Error where

import qualified Data.Text             as Text
import           LambdaCube.Common.Ast

termVarNotInScope :: Identifier -> a
termVarNotInScope x = error $ "Term variable " <> Text.unpack x <> " is not in scope"

typeVarNotInScope :: Identifier -> a
typeVarNotInScope p = error $ "Type variable " <> Text.unpack p <> " is not in scope"

leftoverMetaVar :: MetaIdentifier -> a
leftoverMetaVar h = error $ "Meta variable " <> h <> " is not spliced in"

impossibleCase :: a
impossibleCase = error "Impossible case occurs; This is a bug of the implementation"

notTypeChecked :: a
notTypeChecked = error "Invalid runtime type appears; Did you type check this?"

elaborationBug :: a
elaborationBug = error "Invalid elaboration. This can be a bug of the elaborator"

argumentTypeError :: a
argumentTypeError = error "Function argument type mismatch"
