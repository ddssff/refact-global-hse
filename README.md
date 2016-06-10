# What it does

 * Moves declarations between modules
 * Moves the exports of those declarations
 * Adds imports to the arrival module to support the new declarations
 * Update imports in all known modules to reflect the declaration's new location
 * Maybe import the symbols of the departing declaration if still referenced
 * Copy LANGUAGE pragmas from the departure module to the arrival module
 * Clean up import lists using ghc's -ddump-minimal-imports flag
 * Retain all comments and white space outside of the import list

# What it might do soon

 * Handle CPP directives
 * Move a declaration to a particular line of a module (rather than the end)
 * Find symbols that are exported but never imported and make them local
 * Minimize LANGUAGE directives by trial and error
 * Remove modules that become empty
 * Fix up qualified references to declarations that moved

# What it might do eventually

 * Figure out what imports are required by a template haskell splice (and add them)
 * Figure out what exports are created by a template haskell splice (and move them)
 * Update your .cabal file when modules appear or disappear

# Requirements

 The only requirement currently not in hackage is filemanip >= 0.3.6.4,
 you may get it from https://github.com/ddssff/filemanip.  It fixes a
 bug where the find function doesn't notice working directory changes.

# How To

 1. There are scripts to do simple import cleaning and declaration moving:

        $ runhaskell scripts/Clean.hs --top=/path/to/somerepo --find=Foo
        $ runhaskell scripts/Move.hs --help

 (Does not handle instances.)

 2. Use the interpreter.  Best examples are in the test suite.  Make sure
    your file permissions are set up so you can read the .ghci script:

        $ chmod g-w . .ghci
        $ ghci
        λ runSimpleMove "/path/to/somerepo" (moveDeclsByName "funcname1" "Foo.OldMod" "Foo.NewMod" <>
                                             moveDeclsByName "funcname2" "Foo.OldMod" "Foo.NewMod")

# About reformatted import lists

I know many people have particular opinions about the formatting of import
lists.  Unfortunately, this tool will reformat your import list in a very
specific way, which is probably not exactly what you prefer.  However,
consider that things are different when you have a tool maintaining your
import list, and the generated format has some advantages.  The modules are
alphabetized, and the symbols are also alphabetized and fully expanded.
This makes locating symbols very easy, and lets you know if you need to
insert a new one.  If you need a symbol that isn't imported and know what
module it is in, just delete the symbol list and start using the symbol.
The tool will find it for you the next time it runs.

# Answers to unasked questions

  1. EMPTY IMPORT LISTS - when an import list becomes empty it is not
     necessarily safe to remove it - it may be importing necessary
     instances.  So at present it doesn't.

  2. Sometimes you can avoid circular imports by moving a declaration to
     a temporary module and then to where you actually wanted it.

# To Do

  1. Add ways to break circular imports.  For example, find additional decls
     so that moving the whole group does not leave dangling references.
  3. Distinguish what exactly each test case tests
  7. Whitespace issues - e.g. a comment directly following a decl should stay with
     that decl.
  8. Finish the HasSymbols and HasCNames instances in Symbols.hs.  Retire FoldDeclared.
  9. Port from haskell-src-exts to ghc-exactprint.
