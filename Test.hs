import Buffer
import Metric
import Test.HUnit
import qualified Data.ByteString.Char8 as SB

data DummyState = DS

instance IMetricState DummyState where
  combine _ _ = DS
  toResults _ = undefined

someTime :: SB.ByteString
someTime = SB.pack "2012-07-05 09:00:00"

someLaterTime :: SB.ByteString
someLaterTime = SB.pack "2012-07-05 09:00:01"

tests :: Test
tests = test [ "Insert into empty buffer" ~: size (insertIntoBuf DS empty someTime) @?= 1
              , "Empty buffer is empty" ~: size empty @?= 0
              , "Insert same timestamp twice combines" ~: size (insertIntoBuf DS (insertIntoBuf DS empty someTime) someTime) @?= 1
              , "Insert different timestamps extends" ~: size (insertIntoBuf DS (insertIntoBuf DS empty someTime) someLaterTime) @?= 2
              , "Insert older timestamp extends" ~: size (insertIntoBuf DS (insertIntoBuf DS empty someLaterTime) someTime) @?= 2
              ]