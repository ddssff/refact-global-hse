-- A module copied from hse-cpp, but the return value from
-- parseFileWithCommentsAndCPP includes the preprocessed text.
-- Hopefully I won't need this, but it is useful for debugging.
module CPP
  ( parseFileWithCommentsAndCPP
  , parseFileContentsWithCommentsAndCPP
  , defaultCpphsOptions
  , CpphsOptions(..)
  , BoolOptions(..)
  ) where

import Data.List (isSuffixOf)
import Language.Haskell.Exts.Annotated (ParseMode(baseLanguage, extensions, ignoreLanguagePragmas, parseFilename), Comment, KnownExtension(CPP), impliesExts, Module, parseModuleWithComments, ParseResult, readExtensions, SrcSpanInfo, toExtensionList)
-- import Language.Preprocessor.Cpphs (BoolOptions(..), CpphsOptions(..), runCpphs)
import Language.Preprocessor.Cpphs hiding (defaultCpphsOptions)
import qualified Language.Preprocessor.Cpphs as Orig (defaultCpphsOptions)
import Language.Preprocessor.Unlit (unlit)

parseFileWithCommentsAndCPP ::  CpphsOptions -> ParseMode -> FilePath
                      -> IO (ParseResult (Module SrcSpanInfo, [Comment], String))
parseFileWithCommentsAndCPP cppopts parseMode0 file = do
    content <- readFile file
    parseFileContentsWithCommentsAndCPP cppopts parseMode content
  where
    parseMode = parseMode0 { parseFilename = file }

parseFileContentsWithCommentsAndCPP
    :: CpphsOptions -> ParseMode -> String
    -> IO (ParseResult (Module SrcSpanInfo, [Comment], String))
parseFileContentsWithCommentsAndCPP cppopts p rawStr = do
    let file = parseFilename p
        md = delit file rawStr
        cppMode = updateExtensions p md
    processedSrc <- cpp cppopts cppMode md
    let finalMode = updateExtensions cppMode processedSrc
    return $ parseModuleWithComments finalMode processedSrc >>= \(m, cs) -> pure (m, cs, processedSrc)

updateExtensions :: ParseMode -> String -> ParseMode
updateExtensions p modname =
  let oldLang = baseLanguage p
      exts = extensions p
      (bLang, extraExts) =
          case (ignoreLanguagePragmas p, readExtensions modname) of
            (False, Just (mLang, es)) ->
                 (case mLang of {Nothing -> oldLang;Just newLang -> newLang}, es)
            _ -> (oldLang, [])
  in p { extensions = exts ++ extraExts
       , ignoreLanguagePragmas = False
       , baseLanguage = bLang
       }

cpp :: CpphsOptions -> ParseMode -> String -> IO String
cpp cppopts p str
  | CPP `elem` impliesExts (toExtensionList (baseLanguage p) (extensions p))
  = runCpphs cppopts (parseFilename p) str
  | otherwise = return str

delit :: String -> String -> String
delit fn = if ".lhs" `isSuffixOf` fn then unlit fn else id

defaultCpphsOptions :: CpphsOptions
defaultCpphsOptions =
  Orig.defaultCpphsOptions
  { boolopts = (boolopts Orig.defaultCpphsOptions)
      { locations = True
      , stripC89 = True
      , stripEol = False
      , hashline = False
      }
  }
