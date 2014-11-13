module Main where
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator

integer :: Parser Int
integer = (many1 digit) >>= (\s -> return (read s :: Int))

parens  = between (string "(") (string ")")

expr = term `chainl1` addop
term    = factor `chainl1` mulop
factor  = parens expr <|> integer

mulop :: Parser (Int -> Int -> Int)
mulop =  (string "*" >> return (*) ) <|>
         (string "/" >> return (div) )

addop :: Parser (Int -> Int -> Int)
addop = (string "+" >> return (+) ) <|>
        (string "-" >> return (-) )

calc str = let it = parse expr "" $ filter (not . flip elem [' ', '\t']) str
           in  case it of
             Right n -> str ++ " => "++ show n
             _ -> "Invalid expr: " ++ str

each_line f = unlines . map f . lines

main = interact $ each_line calc
