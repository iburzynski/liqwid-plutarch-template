module Jambhalarch.Utils (
  DataExport,
  ExportTemplate (..),
  JambExports,
  TestTree,
  applyArg,
  applyPlutarchTypedScript,
  convertTypedScript,
  defExports,
  export,
  mkTypedPolicyPlutarch,
  mkTypedValidatorPlutarch,
  mkTypedValidatorPlutarchTypedScript,
  notImplemented,
  testNoErrors,
  toJSONfile,
  toTypedScript,
  toTypedValidator,
) where

import Cardano.Simple.PlutusLedgerApi.V1.Scripts (PlutarchTypedScript, applyPlutarchTypedScript, mkPlutarchTypedScript)
import Data.Default (def)
import Data.Text (Text)
import Jambhalarch.CLI.Export (
  DataExport,
  ExportTemplate (..),
  JambExports,
  TestTree,
  defExports,
  export,
  notImplemented,
  toJSONfile,
 )
import Plutarch.Api.V2 (PValidator)
import Plutarch.Prelude (ClosedTerm, type (:-->))
import Plutus.Model.Contract (testNoErrors)
import Plutus.Model.V2 (
  TypedValidator,
  mkTypedPolicyPlutarch,
  mkTypedValidatorPlutarch,
  mkTypedValidatorPlutarchTypedScript,
 )

toTypedValidator :: ClosedTerm PValidator -> Either Text (TypedValidator datum redeemer)
toTypedValidator = mkTypedValidatorPlutarch def

toTypedScript :: ClosedTerm a -> Either Text (PlutarchTypedScript a)
toTypedScript = mkPlutarchTypedScript def

applyArg :: ClosedTerm a -> PlutarchTypedScript (a :--> b) -> Either Text (PlutarchTypedScript b)
applyArg term script = applyPlutarchTypedScript def script term

convertTypedScript :: PlutarchTypedScript PValidator -> TypedValidator datum redeemer
convertTypedScript = mkTypedValidatorPlutarchTypedScript