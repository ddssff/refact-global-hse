{-# LANGUAGE FlexibleInstances #-}

module ImportTests where

import Data.Default (Default(def))
import Data.List (intercalate)
import Imports (mergeDecls)
import Language.Haskell.Exts
import Test.HUnit
import Utils (prettyPrint')

importTests :: Test
importTests = TestList [import1, import2]

instance Default (ImportDecl ()) where
    def = ImportDecl { importAnn = ()
                     , importModule = ModuleName () "Prelude"
                     , importQualified = False
                     , importSrc = False
                     , importSafe = False
                     , importPkg = Nothing
                     , importAs = Nothing
                     , importSpecs = Nothing }

import1 :: Test
import1 = TestCase $ do
            assertEqual "import1" expected actual
    where
      expected = def { importModule = ModuleName () "Data.Map.Strict"
                     , importAs = Just (ModuleName () "Map")
                     , importSpecs = Just (ImportSpecList () False [IVar () (Ident () "adjust")]) }
      actual = fmap (const ()) $ fromParseResult $ parseImportDecl "import Data.Map.Strict as Map (adjust)"

import2 :: Test
import2 = TestCase $ do
            assertEqual "import2" (unlines (map prettyPrint' expected)) (unlines (map prettyPrint' actual))
    where
      expected = [fromParseResult
                  (parseImportDecl
                   ("import qualified Language.Haskell.Exts.Annotated as A (" ++
                    intercalate ", "
                      ["Annotated(ann)",
                       "Decl(TypeSig)",
                       "ExportSpec(EAbs, EThingAll, EThingWith, EVar)",
                       "ExportSpecList(ExportSpecList)",
                       "ImportDecl(ImportDecl, importModule, importSpecs)",
                       "ImportSpec",
                       "ImportSpecList(ImportSpecList)",
                       "Module(Module)",
                       "ModuleHead(ModuleHead)",
                       "Pretty",
                       "QName(Qual, UnQual)"] ++ ")"))]
      actual = mergeDecls
                  [ fromParseResult
                    (parseImportDecl
                     ("import qualified Language.Haskell.Exts.Annotated as A (" ++
                      intercalate ", "
                        ["QName(Qual, UnQual)",
                         "ImportSpecList(ImportSpecList)",
                         "ImportDecl(ImportDecl, importModule, importSpecs)"] ++ ")"))
                  , fromParseResult
                    (parseImportDecl
                     ("import qualified Language.Haskell.Exts.Annotated as A (" ++
                      intercalate ", "
                        ["ImportSpec",
                         "ModuleHead(ModuleHead)",
                         "ImportDecl(ImportDecl, importModule, importSpecs)",
                         "ExportSpecList(ExportSpecList)"] ++ ")"))
                  , fromParseResult
                    (parseImportDecl
                     ("import qualified Language.Haskell.Exts.Annotated as A (" ++
                      intercalate ", "
                        ["ExportSpec(EAbs, EThingAll, EThingWith, EVar)",
                         "Decl(TypeSig)",
                         "Module(Module)",
                         "Annotated(ann)",
                         "Pretty"] ++ ")"))
                  ]
