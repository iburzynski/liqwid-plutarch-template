module Contracts.Samples.Simple where

import Jambhalarch.Utils
import Plutarch
import Plutarch.Api.V1 (PRedeemer)
import Plutarch.Api.V2
import Plutarch.Prelude

gift :: Term s (PAsData PDatum :--> PAsData PRedeemer :--> PAsData PScriptContext :--> PUnit)
gift = plam $ \_datm _redm _ctx -> pconstant ()

giftTest :: TestTree
giftTest = notImplemented

dataExports :: [DataExport]
dataExports = [() `toJSONfile` "unit"]

giftExports :: JambExports
giftExports =
  export
    (defExports "gift" gift)
      { dataExports
      , test = giftTest
      }

burn :: Term s (PAsData PDatum :--> PAsData PRedeemer :--> PAsData PScriptContext :--> PUnit)
burn = plam $ \_datm _redm _ctx -> perror

burnTest :: TestTree
burnTest = notImplemented

burnExports :: JambExports
burnExports =
  export
    (defExports "burn" burn)
      { test = burnTest
      }
