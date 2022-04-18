module GeneratorIO where

import Parser
import Grammar
import Data.HashMap.Strict ((!))
import qualified Data.HashMap.Strict as H
import Data.List
import Control.Applicative

--import qualified Data.ByteString.Char8 as D
import Control.Monad.State
import Data.STRef
import Control.Monad

data RuleState = Unseen | Parsing | Ready deriving (Show, Eq, Enum)


mkParserLiteral :: Literal -> IO (Parser Literal)
mkParserLiteral (Literal l) = do
    putStrLn $ "Literal: " ++ show l
    return $ Literal <$> prefixP l

mkParserRuleName :: RuleName -> IO (Parser RuleName)
mkParserRuleName (RuleName r) = do
    putStrLn $ "RuleName: " ++ show r
    return $ RuleName <$> prefixP r

mkParserTerm :: GSyntax -> Term  -> IO (Parser Term)
mkParserTerm _ (LTerm literal) = do
    pl      <- mkParserLiteral literal
    putStrLn $ "LTerm: " ++ show literal
    return   $ LTerm <$> pl
mkParserTerm gsyntax (RTerm rulename) = do
    let
        expr' = find' gsyntax rulename
        find' gsyntax rulename = let t = [e | (Rule y e) <- gsyntax, y == rulename] in
            if null t then
                error $ "Something went wrong: " ++ show rulename ++ " | \n " ++ show gsyntax
                else head t
    s <- mapM (mkParserExPart gsyntax) expr'
    putStrLn $ "RuleName: " ++ show rulename ++ ", " ++ "Expr: " ++ show expr'
    return $ RExpr <$> listAltP s


mkParserExPart :: GSyntax -> ExpressionPart -> IO (Parser ExpressionPart)
mkParserExPart g terms = do
    putStrLn $ "Terms: " ++ show terms
    sequenceA <$> forM terms (mkParserTerm g)
    -- traverse (mkParserTerm g)

mkParserExpression :: GSyntax -> Expression -> IO (Parser Expression)
mkParserExpression g eprps = do
    t <- listAltP <$> mapM (mkParserExPart g) eprps
    putStrLn $ "Eparts: " ++ show eprps 
    return $ (:[]) <$> t 
    --(:[]) <$> (listAltP <$> mapM (mkParserExPart g) eprps)

mkParserRule :: GSyntax -> Rule -> IO (Parser Rule)
mkParserRule g s@(Rule rname rexpr) = do
    print s
    t <- mkParserExpression g rexpr
    return $ Rule rname <$> t 


generateParser :: GSyntax -> IO (Parser GSyntax)
generateParser g = do
    t <- mapM (mkParserRule g) g
    let s = listAltP t
    return $ (:[]) <$> s
    --traverse (mkParserRule g) g
    -- let unrule (Rule (RuleName t) _) = t
    --     initState = H.fromList $ zip
    --         (map unrule g)
    --         (repeat (Unseen, Nothing :: Maybe (Parser Expression)))

    --     in 
    --         flip evalState initState $ do
    --             g' <- forM g $ \rule -> do
    --                 rstate <- get
    --                 let rname = unrule rule
    --                     rexpr = case rule of
    --                         Rule _ expr -> expr
    --                 when (fst (rstate ! rname) == Unseen) $ do
    --                     modify' (H.insert rname (Parsing, Nothing))
    --                 rexpr' <- traverse <$> forM rexpr $ \epart -> do
    --                     epart' <- forM epart $ \term -> do
    --                         case term of
    --                             LTerm l -> mkParserLiteral l
    --                             RTerm (RuleName r) ->
    --                                 if 
    --                     return $ traverse epart'
    --                 pure rule
    --             return (pure g)



    --     mkParser = undefined 
    -- in evalState (mkParser grammarRules) initState
    -- runST $ do
    -- ref <- newSTRef $ H.fromList $ zip
    --         (map (\(Rule (RuleName t) _) -> t) grammarRules)
    --         (repeat (Unseen, Nothing))
    -- listAltP <$> mapM (mkParserR ref) grammarRules
    -- --return $ listAltP parsedRulesList
    -- where
    --     --extractRuleName (RuleName  x) =

    --     mkParserR ref (Rule (RuleName rname) expressions)
    --         | null expressions = return empty
    --         | otherwise = listAltP <$> mapM (mkParserE ref) expressions
    --     mkParserE ref terms = do
    --         case terms of
    --             _ -> return empty 
    -- traverse (mkParserRule g) g
                