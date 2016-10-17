import Options.Applicative
import CleanImports
import MoveDecls
import Decorate

data Action
    = CleanImports CleanImports.Params
    | MoveDecls MoveDecls.Params
    | Decorate Decorate.Params

options :: Parser Action
options =
    subparser $
      (command "clean" (clean `withInfo` "Clean the import lists of one or more modules")) <>
      (command "move" (move `withInfo` "Move declarations between modules")) <>
      (command "decorate" (decorate `withInfo` "Illustrate the parse result of a source module"))
    where
      -- clean :: ParserInfo Action
      clean = CleanImports <$> CleanImports.options
      -- move :: ParserInfo Action
      move = MoveDecls <$> MoveDecls.options
      -- decorate :: ParserInfo Action
      decorate = Decorate <$> Decorate.options

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

main :: IO ()
main = execParser (Main.options `withInfo` "Global refactoring tools") >>= go
    where
      go (CleanImports p) = CleanImports.go p
      go (MoveDecls p) = MoveDecls.go p
      go (Decorate p) = Decorate.go p
