{-# LANGUAGE OverloadedStrings #-}
module StackTrace.Plugin (plugin) where
  
import GhcPlugins
import TcRnTypes (IfM, TcM, TcGblEnv, tcg_binds, tcg_rn_decls)
import HsExtension (GhcTc, GhcRn)
import HsDecls (HsGroup)
import HsExpr (LHsExpr)
import HsSyn

import Data.Maybe

plugin :: Plugin
plugin = defaultPlugin
  { parsedResultAction = parsedPlugin
  }

parsedPlugin :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
parsedPlugin _ _ pm = do
  dflags <- getDynFlags

  let extract = hsmodImports . unLoc
      addGHCStackMod = noLoc $ simpleImportDecl $ mkModuleName "GHC.Stack"

      m = updateHsModule addGHCStackMod updateHsmodDecl <$> hpm_module pm
      pm' = pm { hpm_module = m }

  return pm'

updateHsModule :: LImportDecl GhcPs -> (LHsDecl GhcPs -> LHsDecl GhcPs) -> HsModule GhcPs -> HsModule GhcPs
updateHsModule importDecl update hsm = hsm
    { hsmodImports = importDecl:decls
    , hsmodDecls = map update lhss
    }
  where
    decls = hsmodImports hsm
    lhss = hsmodDecls hsm

--------------

updateHsmodDecl :: LHsDecl GhcPs -> LHsDecl GhcPs
updateHsmodDecl = fmap updateHsDecl

updateHsDecl :: HsDecl GhcPs -> HsDecl GhcPs
updateHsDecl (SigD xSig s) = SigD xSig (updateSig s)
updateHsDecl decl = decl

updateSig :: Sig GhcPs -> Sig GhcPs
updateSig (TypeSig xSig ls t) = TypeSig xSig ls (updateLHsSigWcType t)
updateSig sig = sig

updateLHsSigWcType :: LHsSigWcType GhcPs -> LHsSigWcType GhcPs
updateLHsSigWcType lhs@HsWC{} = lhs { hswc_body = updateLHsSigType (hswc_body lhs) }
updateLHsSigWcType lhs@XHsWildCardBndrs{} = lhs

updateLHsSigType :: LHsSigType GhcPs -> LHsSigType GhcPs
updateLHsSigType lhs@HsIB{} = lhs { hsib_body = updateLHsType (hsib_body lhs )}
updateLHsSigType lhs@XHsImplicitBndrs{} = lhs

updateLHsType :: LHsType GhcPs -> LHsType GhcPs
updateLHsType = fmap updateHsType

-- Main process
updateHsType :: HsType GhcPs -> HsType GhcPs
updateHsType ty@(HsQualTy xty ctxt body) = HsQualTy xty (fmap appendHSC ctxt) body
updateHsType ty@HsTyVar{}   = HsQualTy noExt (noLoc $ appendHSC []) (noLoc ty)
updateHsType ty@HsAppTy{}   = HsQualTy noExt (noLoc $ appendHSC []) (noLoc ty)
updateHsType ty@HsFunTy{}   = HsQualTy noExt (noLoc $ appendHSC []) (noLoc ty)
updateHsType ty@HsListTy{}  = HsQualTy noExt (noLoc $ appendHSC []) (noLoc ty)
updateHsType ty@HsTupleTy{} = HsQualTy noExt (noLoc $ appendHSC []) (noLoc ty)
updateHsType ty = ty

appendHSC :: HsContext GhcPs -> HsContext GhcPs
appendHSC cs = mkHSC : cs

-- make HasCallStack => constraint
mkHSC :: LHsType GhcPs
mkHSC = noLoc $ HsTyVar noExt NotPromoted lId

lId :: Located (IdP GhcPs)
lId = noLoc $ mkRdrUnqual $ mkClsOcc "HasCallStack"