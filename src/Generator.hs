module Generator where

import Parser
import Grammar
import Data.HashMap.Strict ((!))
import qualified Data.HashMap.Strict as H
import Data.List
import Control.Applicative

--import qualified Data.ByteString.Char8 as D
import Control.Monad.State
import Data.STRef

data RuleState = Unseen | Parsing | Ready deriving (Show, Eq, Enum)


mkParserLiteral :: Literal -> Parser Literal
mkParserLiteral (Literal l) = Literal <$> prefixP l

mkParserRuleName :: RuleName -> Parser RuleName
mkParserRuleName (RuleName r) = RuleName <$> prefixP r

mkParserTerm :: GSyntax -> Term  -> Parser Term
mkParserTerm _ (LTerm literal) = LTerm <$> mkParserLiteral literal
mkParserTerm gsyntax (RTerm rulename) = RExpr <$> listAltP s
    where
        s = map (mkParserExPart gsyntax) expr'
        expr' = find' gsyntax rulename
        find' gsyntax rulename = let t = [e | (Rule y e) <- gsyntax, y == rulename] in
            if null t then 
                error $ "Something went wrong: " ++ show rulename ++ " | \n " ++ show gsyntax
                else head t

mkParserExPart :: GSyntax -> ExpressionPart -> Parser ExpressionPart
mkParserExPart g = traverse (mkParserTerm g)

mkParserExpression :: GSyntax -> Expression -> Parser Expression
mkParserExpression g eprps = (:[]) <$> listAltP (map (mkParserExPart g) eprps)

mkParserRule :: GSyntax -> Rule -> Parser Rule
mkParserRule g (Rule rname rexpr) = Rule rname <$> mkParserExpression g rexpr



generateParser :: GSyntax -> Parser GSyntax
generateParser g =
    let p = traverse (mkParserRule g) g
        in p `seq` (:[]) <$> listAltP (map (mkParserRule g) g)