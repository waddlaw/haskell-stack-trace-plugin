{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module StackTrace.Plugin (plugin) where

import Control.Arrow (first)
import Data.Monoid (Any(Any, getAny))
import GhcPlugins
#if __GLASGOW_HASKELL__ >= 810
import GHC.Hs
#endif
#if __GLASGOW_HASKELL__ < 810
import HsSyn
#endif

type Traversal s t a b
   = forall f. Applicative f =>
                 (a -> f b) -> s -> f t

type Traversal' s a = Traversal s s a a

plugin :: Plugin
plugin = defaultPlugin {parsedResultAction = parsedPlugin, pluginRecompile = purePlugin}

parsedPlugin ::
     [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
parsedPlugin _ _ pm = do
  let m = updateHsModule <$> hpm_module pm
      pm' = pm {hpm_module = m}
  return pm'

-- Use qualified import for GHC.Stack as "AutoImported.GHC.Stack"
-- ...this should not interfere with other imports...
ghcStackModuleName :: ModuleName
ghcStackModuleName = mkModuleName "AutoImported.GHC.Stack"

#if __GLASGOW_HASKELL__ < 810
importDeclQualified :: Bool
importDeclQualified = True
#else
importDeclQualified :: ImportDeclQualifiedStyle
importDeclQualified = QualifiedPre
#endif

ghcStackImport :: Located (ImportDecl (GhcPass p))
ghcStackImport =
  noLoc $
  (simpleImportDecl $ mkModuleName "GHC.Stack")
    {ideclQualified = importDeclQualified, ideclAs = Just $ noLoc ghcStackModuleName}

updateHsModule :: HsModule GhcPs -> HsModule GhcPs
updateHsModule hsm =
  hsm {hsmodImports = hsmodImports', hsmodDecls = hsmodDecls'}
  where
    -- Traverse the haskell AST; if we have to add some HasStack
    -- constraint we set a flag in a (Any,) functor.
    -- ...it'd be simpler to check if before == after, but Haskell AST
    -- doesn't have Eq instances.
    (updatedP, hsmodDecls') =
      first getAny $
      (traverse . astTraversal) updateHsType (hsmodDecls hsm)

    -- Only import GHC.Stack if needed for a constraint we introduced
    hsmodImports' =
      (if updatedP
         then [ghcStackImport]
         else []) ++
      hsmodImports hsm

    astTraversal :: Traversal' (LHsDecl GhcPs) (HsType GhcPs)
    astTraversal = updateHsmodDecl
                 . updateHsDecl
                 . updateSig
                 . updateLHsSigWsType
                 . updateLHsSigType
                 . updateLHsType

--------------
updateHsmodDecl :: Traversal' (LHsDecl GhcPs) (HsDecl GhcPs)
updateHsmodDecl = traverse

updateHsDecl :: Traversal' (HsDecl GhcPs) (Sig GhcPs)
updateHsDecl f (SigD xSig s) = SigD xSig <$> f s
updateHsDecl _ sig = pure sig

updateSig :: Traversal' (Sig GhcPs) (LHsSigWcType GhcPs)
updateSig f (TypeSig xSig ls t) = TypeSig xSig ls <$> f t
updateSig _ sig = pure sig

updateLHsSigWsType :: Traversal' (LHsSigWcType GhcPs) (LHsSigType GhcPs)
updateLHsSigWsType f lhs@HsWC {} =
  (\x -> lhs {hswc_body = x}) <$> f (hswc_body lhs)
updateLHsSigWsType _ lhs = pure lhs

updateLHsSigType :: Traversal' (LHsSigType GhcPs) (LHsType GhcPs)
updateLHsSigType f lhs@HsIB {} =
  (\x -> lhs {hsib_body = x}) <$> f (hsib_body lhs)
updateLHsSigType _ lhs = pure lhs

updateLHsType :: Traversal' (LHsType GhcPs) (HsType GhcPs)
updateLHsType = traverse

-- Main process
updateHsType :: HsType GhcPs -> (Any, HsType GhcPs)
updateHsType (HsQualTy xty ctxt body) =
  flagASTModified $ HsQualTy xty (fmap appendHSC ctxt) body
updateHsType ty@HsTyVar {} =
  flagASTModified $ HsQualTy xQualTy (noLoc $ appendHSC []) (noLoc ty)
updateHsType ty@HsAppTy {} =
  flagASTModified $ HsQualTy xQualTy (noLoc $ appendHSC []) (noLoc ty)
updateHsType ty@HsFunTy {} =
  flagASTModified $ HsQualTy xQualTy (noLoc $ appendHSC []) (noLoc ty)
updateHsType ty@HsListTy {} =
  flagASTModified $ HsQualTy xQualTy (noLoc $ appendHSC []) (noLoc ty)
updateHsType ty@HsTupleTy {} =
  flagASTModified $ HsQualTy xQualTy (noLoc $ appendHSC []) (noLoc ty)
updateHsType ty = pure ty

#if __GLASGOW_HASKELL__ < 810
xQualTy = noExt
#else
xQualTy :: NoExtField
xQualTy = NoExtField
#endif

flagASTModified :: a -> (Any, a)
flagASTModified a = (Any True, a)

appendHSC :: HsContext GhcPs -> HsContext GhcPs
appendHSC cs = mkHSC : cs

-- make HasCallStack => constraint
mkHSC :: LHsType GhcPs
mkHSC = noLoc $ HsTyVar xQualTy NotPromoted lId

lId :: Located (IdP GhcPs)
lId = noLoc $ mkRdrQual ghcStackModuleName $ mkClsOcc "HasCallStack"
