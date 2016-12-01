{-# LANGUAGE Rank2Types, DeriveFunctor #-}

import Control.Comonad
import qualified Control.Foldl as F
import qualified Data.Map.Strict as Map
import Text.Trifecta
import Text.Parser.Expression
import System.Environment
import System.IO (stderr)
import Text.PrettyPrint.ANSI.Leijen (hPutDoc)

data Bucket = MkBucket !Int !Double

instance Eq Bucket where
  MkBucket i _ == MkBucket j _ = i == j

instance Ord Bucket where
  MkBucket i _ <= MkBucket j _ = i <= j

data Val = Dbl !Double
  | Bucket !Bucket
  | Map !(Map.Map Val Val)
  deriving (Eq, Ord)

data Prim = PDbl !Double
  | PBucket !Double !Double !(Expr Prim)
  | PCol !Int

data Aggregation = Sum (Expr Prim)
  | Distrib (Expr Prim) Aggregation
  | Len
  | Expr (Expr Aggregation)

data Expr a = Val a
  | Add (Expr a) (Expr a)
  | Sub (Expr a) (Expr a)
  | Mul (Expr a) (Expr a)
  | Div (Expr a) (Expr a)
  | Pow (Expr a) (Expr a)

parseAggregation :: Parser Aggregation
parseAggregation = choice
  [ Expr <$> parens (parseExpr parseAggregation)
  , Sum <$> (symbol "sum" *> parseTerm prim)
  , symbol "dist" *> (Distrib <$> parseTerm prim <*> parseAggregation)
  , Len <$ symbol "len"
  ]

prim :: Parser Prim
prim = choice
  [ fmap PCol $ symbol "$" *> fmap fromIntegral natural
  , symbol "bucket" *> (PBucket <$> double <*> double <*> parseTerm prim)
  , PDbl <$> double
  ]

parseTerm :: Parser a -> Parser (Expr a)
parseTerm m = choice
  [ parens (parseExpr m)
  , Val <$> m
  ]

parseExpr :: Parser a -> Parser (Expr a)
parseExpr m = buildExpressionParser
  [ [Infix (Pow <$ symbol "^") AssocRight]
  , [Infix (Mul <$ symbol "*") AssocLeft, Infix (Div <$ symbol "/") AssocLeft]
  , [Infix (Add <$ symbol "+") AssocLeft, Infix (Sub <$ symbol "-") AssocLeft]
  ]
  (parseTerm m)

toFold :: Aggregation -> F.Fold [String] Val
toFold (Sum expr) = F.Fold add 0 Dbl where
  add x str = x + asDbl (eval expr str)
toFold (Distrib expr aggr) = F.Fold ins Map.empty (Map . fmap extract) where
  ins m str = Map.alter (Just . (\(F.Fold f x r) -> F.Fold f (f x str) r) . maybe (toFold aggr) id)
    (eval expr str) m
toFold Len = Dbl <$> F.genericLength
toFold (Expr e) = runExpr toFold e

runExpr :: (Applicative f) => (a -> f Val) -> Expr a -> f Val
runExpr k (Val a) = k a
runExpr k (Add a b) = withDbl (+) <$> runExpr k a <*> runExpr k b
runExpr k (Sub a b) = withDbl (-) <$> runExpr k a <*> runExpr k b
runExpr k (Mul a b) = withDbl (*) <$> runExpr k a <*> runExpr k b
runExpr k (Div a b) = withDbl (/) <$> runExpr k a <*> runExpr k b
runExpr k (Pow a b) = withDbl (**) <$> runExpr k a <*> runExpr k b

asDbl :: Val -> Double
asDbl (Dbl a) = a
asDbl _ = error "Expecting Double"

withDbl :: (Double -> Double -> Double) -> Val -> Val -> Val
withDbl f (Dbl a) (Dbl b) = Dbl $ f a b
withDbl _ _ _ = error "Expecting Double"

eval :: Expr Prim -> [String] -> Val
eval = runExpr (\e xs -> case e of
  PCol i -> Dbl $ read $ xs !! (i - 1)
  PBucket i x0 ex -> case eval ex xs of
    Dbl x -> let n = floor $ (x - x0) / i in Bucket
      $ MkBucket n (fromIntegral n * i + x0)
    _ -> error "Expecting Double"
  PDbl v -> Dbl v)

showVal :: Val -> String
showVal (Dbl a) = show a
showVal (Bucket (MkBucket _ x)) = show x
showVal (Map m) = unlines [showVal k ++ " " ++ showVal v | (k, v) <- Map.toList m ]

main :: IO ()
main = do
  args <- getArgs
  case traverse (parseString (Expr <$> parseExpr parseAggregation <* eof) mempty) args of
    Success xs -> getContents >>= putStrLn . unwords . map showVal
      . F.fold (traverse toFold xs) . map words . lines
    Failure e -> hPutDoc stderr (_errDoc e)
