{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.List             (intercalate, isInfixOf)
import Data.Maybe            (catMaybes)
import System.FilePath.Posix (takeFileName)
import Language.C
import Language.C.Data.Ident
import Language.C.System.GCC (newGCC)

path     = "/usr/local/Cellar/ode/0.14/include/ode/ode.h"
fileName = takeFileName path
quote    = "\\\""

data Binding =
  Function { name :: String, returnType :: String, argumentTypes :: [String] }
  deriving (Show, Read, Eq)

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
    Right a -> mapM_ putStrLn $ map haskell $ sizeMapTrans a
    -- Right a -> mapM_ (\b -> print b >> putStrLn "") $ sizeMapTrans a

  putStrLn "  printf(\"}\");"
  putStrLn "}"

haskell (Function { name, returnType, argumentTypes }) = "foreign import ccall \"" ++ name ++ "\" c" ++  name ++ " :: " ++ intercalate " -> " argumentTypes ++ " -> IO " ++ returnType

printField name = putStrLn $ "  printf(\"  " ++ quote ++ name ++ quote ++ ": " ++ "%d,\\n" ++ "\", sizeof(" ++ name ++ "));"

flatMap f as = concat (map f as)

sizeMapTrans (CTranslUnit as node) = catMaybes $ map extract $ filter relevantItem as

relevantItem item =
  case fileOfNode (annotation item) of
    Just path -> "ode" `isInfixOf` path
    Nothing -> False

extract a =
  case a of
    (CDeclExt decl) ->
      case decl of
        (CDecl specs decs _) ->
          case specs of
            [CTypeSpec (CTypeDef (Ident retType _ _) _)] ->
              case decs of
                [(Just (CDeclr (Just (Ident funName _ _)) declr _ _ _), Nothing, Nothing)] ->
                  case declr of
                    [CFunDeclr (Right (args, _)) _ _] ->
                      let typeNames = map extractTypeFromDecl args
                          in Just (Function { name = funName, returnType = retType, argumentTypes = typeNames })
                    _ -> Nothing
                _ -> Nothing
            _ -> Nothing
    _ -> Nothing

extractTypeFromDecl (CDecl [CTypeSpec ty] _ _) =
  case ty of
    CVoidType _                   -> "()"
    CCharType _                   -> "CChar"
    CShortType _                  -> "CShort"
    CIntType _                    -> "CInt"
    CLongType _                   -> "CLong"
    CFloatType _                  -> "CFloat"
    CDoubleType _                 -> "CDouble"
    CSignedType _                 -> "CInt"
    CUnsigType _                  -> "CUInt"
    CBoolType _                   -> "Bool"
    (CTypeDef (Ident name _ _) _) -> cNameToHaskellName name
    _                             -> "<--Unsuported: " ++ show ty ++ "-->"
extractTypeFromDecl a = "<--Unsuported: " ++ show a ++ "-->"

cNameToHaskellName name = "C" ++ name
