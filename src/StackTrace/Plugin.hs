{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module StackTrace.Plugin (plugin) where

import Control.Arrow (first)
import Data.Monoid (Any(Any, getAny))
import GHC.Types.SrcLoc
#if __GLASGOW_HASKELL__ >= 900
import GHC.Plugins
#else

import GhcPlugins
#endif
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
  L srcSpan $
  (simpleImportDecl $ mkModuleName "GHC.Stack")
    { ideclQualified = importDeclQualified, ideclAs = Just $ noLoc ghcStackModuleName }
  where
    -- This is for GHC-9 related problems. @noLoc@ causes GHC to throw warnings
    -- about unused imports. Even if the import is used
    -- See: https://github.com/waddlaw/haskell-stack-trace-plugin/issues/16
    srcSpan = RealSrcSpan (realSrcLocSpan $ mkRealSrcLoc "haskell-stack-trace-plugin:very-unique-file-name-to-avoid-collision" 1 1) Nothing

#if __GLASGOW_HASKELL__ >= 900
updateHsModule :: HsModule -> HsModule
#else
updateHsModule :: HsModule GhcPs -> HsModule GhcPs
#endif
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
                 . updateLHsSigWsType
                 . updateLHsSigType
                 . updateLHsType

--------------
updateHsmodDecl :: Traversal' (LHsDecl GhcPs) (HsDecl GhcPs)
updateHsmodDecl = traverse

updateHsDecl :: Traversal' (HsDecl GhcPs) (LHsSigWcType GhcPs)
updateHsDecl f (SigD xSig s) = SigD xSig <$> updateSig f s
updateHsDecl f (ValD xVal hsBind) = ValD xVal <$> updateHsBind f hsBind
updateHsDecl _ sig = pure sig

updateHsBind :: Traversal' (HsBind GhcPs) (LHsSigWcType GhcPs)
updateHsBind f bind@FunBind {} = (\x -> bind {fun_matches = x}) <$> updateMatchGroup f (fun_matches bind)
updateHsBind _ bind = pure bind

updateMatchGroup :: Traversal' (MatchGroup GhcPs (LHsExpr GhcPs)) (LHsSigWcType GhcPs)
updateMatchGroup f mg@MG {} = (\x -> mg {mg_alts = x}) <$> updateLLMatch f (mg_alts mg)
#if __GLASGOW_HASKELL__ < 900
updateMatchGroup _ mg = pure mg
#endif

updateLocated :: Functor f => (a -> b -> f c) -> a -> Located b -> f (Located c)
updateLocated f g (L l e) = L l <$> f g e

updateLLMatch :: Traversal' (Located [LMatch GhcPs (LHsExpr GhcPs)]) (LHsSigWcType GhcPs)
updateLLMatch = updateLocated updateLMatches

updateLMatches :: Traversal' [LMatch GhcPs (LHsExpr GhcPs)] (LHsSigWcType GhcPs)
updateLMatches f = traverse (updateLocated updateMatch f)

updateMatch :: Traversal' (Match GhcPs (LHsExpr GhcPs)) (LHsSigWcType GhcPs)
updateMatch f m@Match {} = (\x -> m {m_grhss = x}) <$> updateGrhss f (m_grhss m)
#if __GLASGOW_HASKELL__ < 900
updateMatch _ m = pure m
#endif

updateGrhss :: Traversal' (GRHSs GhcPs (LHsExpr GhcPs)) (LHsSigWcType GhcPs)
updateGrhss f grhss@GRHSs {} = (\x -> grhss {grhssLocalBinds = x}) <$> updateLHsLocalBinds f (grhssLocalBinds grhss)
#if __GLASGOW_HASKELL__ < 900
updateGrhss _ grhss = pure grhss
#endif

updateLHsLocalBinds :: Traversal' (LHsLocalBinds GhcPs) (LHsSigWcType GhcPs)
updateLHsLocalBinds = updateLocated updateLocalBinds

updateLocalBinds :: Traversal' (HsLocalBinds GhcPs) (LHsSigWcType GhcPs)
updateLocalBinds f (HsValBinds xHsValBinds hsValBindsLR) = HsValBinds xHsValBinds <$> updateHsValBindsLR f hsValBindsLR
updateLocalBinds _ hsValBinds = pure hsValBinds

updateHsValBindsLR :: Traversal' (HsValBindsLR GhcPs GhcPs) (LHsSigWcType GhcPs)
updateHsValBindsLR f (ValBinds xValBinds lHsBindsLR lSigs) = ValBinds xValBinds lHsBindsLR <$> updateLSigs f lSigs
updateHsValBindsLR _ valBinds = pure valBinds

updateLSigs :: Traversal' [LSig GhcPs] (LHsSigWcType GhcPs)
updateLSigs f = traverse (updateLocated updateSig f)

updateSig :: Traversal' (Sig GhcPs) (LHsSigWcType GhcPs)
updateSig f (TypeSig xSig ls t) = TypeSig xSig ls <$> f t
updateSig _ sig = pure sig

updateLHsSigWsType :: Traversal' (LHsSigWcType GhcPs) (LHsSigType GhcPs)
updateLHsSigWsType f lhs@HsWC {} =
  (\x -> lhs {hswc_body = x}) <$> f (hswc_body lhs)
#if __GLASGOW_HASKELL__ < 900
updateLHsSigWsType _ lhs = pure lhs
#endif

updateLHsSigType :: Traversal' (LHsSigType GhcPs) (LHsType GhcPs)
updateLHsSigType f lhs@HsIB {} =
  (\x -> lhs {hsib_body = x}) <$> f (hsib_body lhs)
#if __GLASGOW_HASKELL__ < 900
updateLHsSigType _ lhs = pure lhs
#endif

updateLHsType :: Traversal' (LHsType GhcPs) (HsType GhcPs)
updateLHsType = traverse

-- Main process
updateHsType :: HsType GhcPs -> (Any, HsType GhcPs)
updateHsType ty@(HsQualTy xty ctxt body) =
  if hasHasCallStack (unLoc ctxt)
    then pure ty
    else flagASTModified $ HsQualTy xty (fmap appendHSC ctxt) body
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
xQualTy :: NoExt
xQualTy = noExt
#else
xQualTy :: NoExtField
xQualTy = NoExtField
#endif

flagASTModified :: a -> (Any, a)
flagASTModified a = (Any True, a)

appendHSC :: HsContext GhcPs -> HsContext GhcPs
appendHSC cs = mkHSC : cs

hasHasCallStack :: HsContext GhcPs -> Bool
hasHasCallStack = any (checkHsType . unLoc)
  where
    checkHsType :: HsType GhcPs -> Bool
    checkHsType (HsTyVar _ _ lid) = unLoc lid == (mkRdrUnqual $ mkClsOcc  "HasCallStack")
    checkHsType _ = False

-- make HasCallStack => constraint
mkHSC :: LHsType GhcPs
mkHSC = noLoc $ HsTyVar xQualTy NotPromoted lId

lId :: Located (IdP GhcPs)
lId = noLoc $ mkRdrQual ghcStackModuleName $ mkClsOcc "HasCallStack"
