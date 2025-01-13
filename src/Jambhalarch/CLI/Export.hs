module Jambhalarch.CLI.Export (
  DataExport,
  ExportTemplate (..),
  JambExports,
  TestTree,
  defExports,
  export,
  notImplemented,
  toJSONfile,
) where

import Jambhalarch.CLI.Types (
  ContractExports (ContractExports),
  ContractName,
  DataExport (..),
  ExportTemplate (..),
  FileName,
  JambExports,
  TestTree,
 )
import Plutarch.Prelude (ClosedTerm)
import PlutusTx (ToData)
import Test.Tasty.HUnit (assertFailure, testCase)

{- | Converts an `ExportTemplate` into a `JambExports` value,
   which can be imported into the `Contracts` module and added to the list of contracts for use with the `jamb` CLI.
-}
export :: ExportTemplate -> JambExports
export ExportTemplate {..} = (contractName, ContractExports scriptExport dataExports test)

{- | The default `ExportTemplate` constructor: includes only the contract script with no data exports or tests.
   The template can optionally be extended using record update syntax, i.e.
   @
   (defExports contract) {
     dataExports = [ myValue `toJSONfile` "my-value"
                   , myOtherValue `toJSONfile` "my-other-value"
                   ],
     test = myTest
     }
   @
-}
defExports :: ContractName -> ClosedTerm a -> ExportTemplate
defExports n c = ExportTemplate n c [] notImplemented

-- | Creates a `DataExport` value to be serialised into a JSON file with the given name.
toJSONfile :: (ToData d) => d -> FileName -> DataExport
d `toJSONfile` filename = DataExport filename d

-- | An empty placeholder test.
notImplemented :: TestTree
notImplemented =
  testCase "Missing Test" (assertFailure "test not implemented")
