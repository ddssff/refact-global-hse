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
 * Move a declaration to a particular line of a module (rather than
   the end)
 * global export minimization - find symbols that are exported but
   never imported and make them local
 * Minimize LANGUAGE directives by trial and error
 * Remove modules that become empty
 * Fix up qualified references to declarations that moved

# What it might do eventually

 * Figure out what imports are required by a template haskell splice (and add them)
 * Figure out what exports are created by a template haskell splice (and move them)
 * Update your .cabal file when modules appear or disappear

# How To

## Run from ghci

    $ ghci
    λ finalParams (set (ghcOpts . hsSourceDirs) ["src"] (set CleanImports.toClean [(Just "src", "CPP.hs")] def)) >>= CleanImports.go
    λ finalParams (over (ghcOpts . hashDefines) (++ [cabalMacro "base" (Version [4,9,0] [])]) $
                   set (ghcOpts . hsSourceDirs) ["src"] $
                   set toClean [(Just "src", "CPP.hs")] $
                   set unsafe True def) >>= CleanImports.go

## Run command line tool

The refactor tool has sub-commands to do import cleaning, declaration
moving, and source code decoration:

  refactor clean --cd /path/to/somerepo --find=Foo
  refactor move --cd /path/to/somerepo --decl funcname1,Foo.OldMod,Foo.NewMod \\
     --decl funcname2,Foo.OldMod,Foo.NewMod

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

# Reasons why refactor move might fail

  1. It created a circular import.  If you move a symbol s from module
     A to module B, but

       (1) declarations in A still reference s, and
       (2) s references symbols still in A,

     then the resulting circular imports will cause the compile to
     fail.  You must pull all the symbols that use s over to B.
     Sometimes you can avoid this problem with circular imports by
     moving a declaration in two steps, first to a temporary module
     and then to where you actually wanted it.  (Note to self - is
     this still true?  Why?)

  2. Two different symbols with the same name may be pulled together by
     a move, and this will (as of right now) cause a compiler error.

  3. EMPTY IMPORT LISTS - when an import list becomes empty it is not
     necessarily safe to remove it - it may be importing necessary
     instances.  So at present it doesn't.

# To Do

  1. Add ways to break circular imports.  For example, find additional decls
     so that moving the whole group does not leave dangling references.
  3. Distinguish what exactly each test case tests
  7. Whitespace issues - e.g. a comment directly following a decl should stay with
     that decl.
  8. Finish the HasSymbols and HasCNames instances in Symbols.hs.  Retire FoldDeclared.
  9. Port from haskell-src-exts to ghc-exactprint.
 10. Add qualifiers to symbols when they need to be disambiguated after a move.
