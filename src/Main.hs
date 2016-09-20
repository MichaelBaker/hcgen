module Main where

import Data.List             (intercalate, isInfixOf)
import System.FilePath.Posix (takeFileName)
import Language.C
import Language.C.Data.Ident
import Language.C.System.GCC (newGCC)

path     = "/usr/local/Cellar/ode/0.14/include/ode/ode.h"
fileName = takeFileName path
quote    = "\\\""

main :: IO ()
main = do
  result <- parseCFile (newGCC "/usr/bin/gcc") Nothing ["-I/usr/local/Cellar/ode/0.14/include", "-U__BLOCKS__"] path
  putStrLn "#include <stdio.h>"
  putStrLn $ "#include \"" ++ path ++ "\""
  putStrLn ""
  putStrLn "void main() {"
  putStrLn "  printf(\"{\\n\");"

  case result of
    Left e  -> error $ "Error: " ++ show e
    Right a -> mapM_ printField $ sizeMapTrans a

  putStrLn "  printf(\"}\");"
  putStrLn "}"

printField name = putStrLn $ "  printf(\"  " ++ quote ++ name ++ quote ++ ": " ++ "%d,\\n" ++ "\", sizeof(" ++ name ++ "));"

flatMap f as = concat (map f as)

sizeMapTrans (CTranslUnit as node) = flatMap sizeMapExtDec as

sizeMapExtDec (CDeclExt a) = sizeMapDecl a
sizeMapExtDec (CFDefExt a) = []
sizeMapExtDec _            = []

sizeMapDecl (CDecl specs ((Just (CDeclr (Just (Ident name _ node)) _ _ _ _),_,_):_) _)
  | any isTypeDef specs = [name]
  | otherwise = []
sizeMapDecl _ = []

isTypeDef (CTypeSpec (CTypeDef _ _)) = True
isTypeDef _                          = False
