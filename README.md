# What it does

 * It moves declarations between modules
 * It globally updates imports of those declarations
 * It minimizes the imports of all modified modules
 * It retains all comments and white space outside of the import list
 * It leaves code in #if alone

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

# Reformatted import lists

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
     instances.  However, by default the import cleaner does remove import
     lists in this case, because the move operations create a lot of these.
     However, it never removes an import lists that starts out empty.
