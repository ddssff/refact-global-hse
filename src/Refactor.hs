module Refactor
    (
    -- * Declaration Moving
      MoveSpec
    , traceMoveSpec
    , moveDeclsAndClean
    , moveDeclsByName
    , moveInstDecls
    , instClassPred
    , moveSpliceDecls
    , splicePred
    -- * Import Cleaning
    , cleanImports
    -- * Module Parsing
    , loadModule, loadModules
    -- * Monad for scanning parsed modules
    , ScanM, scanModule, useComments, keep
    -- * ModuleInfo
    , ModuleInfo(ModuleInfo, _module)
    -- * CPP
    , GHCOpts, ghcOptsOptions, cppOptions, definesL, hsSourceDirs
    -- * Source Code Location
    , endLoc, srcLoc
    -- * Utility Functions
    , gitResetSubdir, withCleanRepo, withCurrentDirectory, withTempDirectory
    ) where

import Refactor.Clean (cleanImports)
import Refactor.CPP (GHCOpts, ghcOptsOptions, cppOptions, definesL, hsSourceDirs)
import Refactor.Decls (moveDeclsAndClean)
import Refactor.LoadModule (loadModule, loadModules)
import Refactor.ModuleInfo (ModuleInfo(..))
import Refactor.MoveSpec (instClassPred, splicePred, moveDeclsByName, moveInstDecls, moveSpliceDecls, MoveSpec, traceMoveSpec)
import Refactor.ScanM (ScanM, scanModule, useComments, keep)
import Refactor.SrcLoc (endLoc, srcLoc)
import Refactor.Utils (gitResetSubdir, withCleanRepo, withCurrentDirectory, withTempDirectory)
