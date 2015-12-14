

module EFA.IO.TableParser (EFA.IO.TableParser.read, write) where

import EFA.IO.TableParserTypes (Map, T(T))
import EFA.IO.Parser (number, eol)

import qualified System.IO as IO

import qualified Data.List as List
import qualified Data.Map as Map

import Text.ParserCombinators.Parsec
          (Parser, skipMany, many, many1, endBy, sepEndBy1, between,
           noneOf, oneOf, char, string, eof, lookAhead, parse, (<|>))

import Control.Applicative (liftA, liftA2, (*>), (<*))
import Control.Monad (forM_, void)


read :: FilePath -> IO (Map Double)
read file = readFile file >>= \txt ->
  case parse tables file txt of
       Right tb -> return tb
       Left err -> ioError $ userError $ show err

tables :: (Read a) => Parser (Map a)
tables = liftA Map.fromList $
  endBy table ((void $ lookAhead double) <|> commentOrEol <|> eof)

table :: (Read a) => Parser (String, T a)
table = do
  skipMany commentOrEol
  (str, dim) <- header
  ds <- many line
  return (str, T dim ds)

header :: Parser (String, (Int, Int))
header = liftA2 (,)
  (sp double *> sp tableName)
  (pair <* commentOrEol)

tableName :: Parser String
tableName = many1 $ noneOf "("

double :: Parser ()
double = string "double" >> separator

pair :: Parser (Int, Int)
pair = between (sp $ char '(') (sp $ char ')') $
  liftA2 (,) (sp number) (sp (char ',') *> sp number)

line :: (Read a) => Parser [a]
line = spacesNeol *> sepEndBy1 number spacesNeol <* commentOrEol

commentOrEol :: Parser ()
commentOrEol = spacesNeol >> (eol <|> comment)

comment :: Parser ()
comment = between (char '#') eol (void $ many neol)

sp :: Parser a -> Parser a
sp = (spacesNeol >>)

spacesNeol :: Parser ()
spacesNeol = skipMany separator

separator :: Parser ()
separator = void $ oneOf "\t "

neol :: Parser ()
neol = void $ noneOf "\n\r"


------------------

writeTable :: IO.Handle -> (String, T Double) -> IO ()
writeTable hdl (name, T xy ds) = do
  let hd = "#1\ndouble " ++ name ++ show xy
      body = List.intercalate "\n" $
               map (List.intercalate " " . map show) ds
  IO.hPutStr hdl ("\n" ++ hd ++ "\n" ++ body ++ "\n")

write :: FilePath -> Map Double -> IO ()
write file tm = IO.withFile file IO.WriteMode $
  forM_ (Map.toList tm) . writeTable
