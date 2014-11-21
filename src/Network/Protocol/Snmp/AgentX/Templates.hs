module Network.Protocol.Snmp.AgentX.Templates where

import Network.Protocol.Snmp.AgentX.Monads
import Network.Protocol.Snmp.AgentX.Types
import Network.Snmp.Client (oidFromBS)
import Data.ByteString.Char8 (pack)
import Language.Haskell.TH
import Language.Haskell.TH.Quote

{--
 i need something like:
base 1.3.6.1.4.1.44729 Fixmon
object top 0
  object middle 0
    object-type name  0 (String "current time")
    object-type time  1 GenerateTime
  object interfaces 1 GenerateInterfaces
--}
--
oidGen =
    QuasiQuoter
    { quoteDec = undefined
    , quoteExp = makeBase
    , quotePat = undefined
    , quoteType = undefined
    }


makeBase :: String -> Q Exp
makeBase s = do
    let [b, oi, name] = words s
        oid = map (LitE . IntegerL) $ oidFromStr oi
    return $ AppE (AppE (AppE (ConE (mkName "Base")) 
                (ListE oid)) 
                (LitE (StringL name))) 
                (ConE (mkName "ZeroValues"))
             

oidFromStr :: String -> [Integer]
oidFromStr = oidFromBS . pack 

str = unlines [ "base 1.3.6.1.4.1.44729 Fixmon"
              , "object top 0"
              , "  object middle 0"
              , "    object-type name  0 (String \"current time\")"
              , "    object-type name  1 (String \"current time\")"
              , "    object-type name  2 (String \"current time\")"
              , "  object middle 1"
              ]

lineToLevel :: String -> (Int, String)
lineToLevel str = 
    let (i, s) = span (== ' ') str
    in (length i, s)

