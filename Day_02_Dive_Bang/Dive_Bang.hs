module DiveBang where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import System.IO

data NavCom
    = Forward Integer
    | Up Integer
    | Down Integer
    deriving (Eq, Show)

type Position = (Integer, Integer, Integer)

main :: IO ()
main = do
    handle <- openFile "Input.txt" ReadMode
    contents <- hGetContents handle
    let (Right input) = runParser navcoms "" contents
    print $ "Part 1: " ++ show (part1 input)
    print $ "Part 2: " ++ show (part2 input)
    hClose handle

-- Parser
type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer :: Parser Integer
integer = lexeme L.decimal

navcom :: Parser NavCom
navcom = operator <*> integer
    where operator = lexeme $ choice [ Forward <$ string' "forward"
                                     , Up      <$ string' "up"
                                     , Down    <$ string' "down" ]

navcoms :: Parser [NavCom]
navcoms = manyTill navcom eof

-- Part 1
moveSimple :: NavCom -> Position -> Position
moveSimple (Forward val) (horizontal, depth, aim) = (horizontal + val, depth, aim)
moveSimple (Up val)      (horizontal, depth, aim) = (horizontal, depth - val, aim)
moveSimple (Down val)    (horizontal, depth, aim) = (horizontal, depth + val, aim)

navigate :: (NavCom -> Position -> Position) -> [NavCom] -> Position
navigate move = foldr move (0, 0, 0)

solution :: (NavCom -> Position -> Position) -> [NavCom] -> Integer
solution movef navcoms =
    let navcoms' = reverse navcoms
        (horizontal, depth, _) = navigate movef navcoms'
    in horizontal * depth

part1 :: [NavCom] -> Integer
part1 = solution moveSimple

-- Part 2
moveAim :: NavCom -> Position -> Position
moveAim (Forward val) (horizontal, depth, aim) = (horizontal + val, depth + aim * val, aim)
moveAim (Up val)      (horizontal, depth, aim) = (horizontal, depth, aim - val)
moveAim (Down val)    (horizontal, depth, aim) = (horizontal, depth, aim + val)

part2 :: [NavCom] -> Integer
part2 = solution moveAim
