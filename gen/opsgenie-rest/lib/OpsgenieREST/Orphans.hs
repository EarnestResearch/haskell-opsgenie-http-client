{-# LANGUAGE FlexibleInstances #-}

module OpsgenieREST.Orphans where

import qualified Prelude as P

instance P.MonadFail (P.Either P.String) where
  fail = P.Left
