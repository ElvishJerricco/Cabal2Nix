{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cabal2Nix.Plan
where

import           Cabal2Nix.Util                           ( quoted
                                                          , bindPath
                                                          )
import           Data.HashMap.Strict                      ( HashMap )
import qualified Data.HashMap.Strict           as Map
import           Data.Text                                ( Text )
import qualified Data.Text                     as Text
import           Nix.Expr

type Version = Text
type Revision = Text -- Can be: rNUM, cabal file sha256, or "revision"

data Plan = Plan
  { packages :: HashMap Text Package
  , compilerVersion :: Text
  , compilerPackages :: HashMap Text Version
  }

data Package = Package
  { packageVersion :: Version
  , packageRevision :: Maybe Revision
  , packageFlags :: HashMap Text Bool
  }

plan2nix :: Plan -> NExpr
plan2nix (Plan { packages, compilerVersion, compilerPackages }) =
  mkFunction "hackage"
    . mkNonRecSet
    $ [ "packages" $= (mkNonRecSet $ uncurry bind =<< Map.toList packages)
      , "compiler" $= mkNonRecSet
        [ "version" $= mkStr compilerVersion
        , "nix-name" $= mkStr ("ghc" <> Text.filter (/= '.') compilerVersion)
        , "packages" $= mkNonRecSet (uncurry bind' <$> Map.toList compilerPackages)
        ]
      ]
 where
  bind pkg (Package { packageVersion, packageRevision, packageFlags }) =
    let verExpr      = mkSym "hackage" !. pkg !. packageVersion
        revExpr      = maybe verExpr (verExpr !.) packageRevision
        revBinding   = bindPath [quoted pkg, "revision"] revExpr
        flagBindings = Map.foldrWithKey
          (\fname val acc -> bindPath [quoted pkg, "flags", fname] (mkBool val) : acc)
          []
          packageFlags
    in  revBinding : flagBindings
  bind' pkg ver = quoted pkg $= mkStr ver
