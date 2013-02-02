import Buffer
import Metrics.Common
import Test.HUnit
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Either
import System.Exit

data DummyState = DS deriving (Show, Eq)

instance IMetricState DummyState where
  combine _ _ = DS
  toResults a _ = [("", "", a)]

dummyMetric :: a -> DummyState
dummyMetric _ = DS

dummyInput = map B.pack [ "2012-07-05 09:00:00,759"
                        , "2012-07-05 09:00:01,764"
                        , "2012-07-05 09:00:02,765"
                        , "2012-07-05 09:00:00,801"
                        , "2012-07-05 09:00:03,765"
                        ]
             
someTime :: SB.ByteString
someTime = SB.pack "2012-07-05 09:00:00"

someLaterTime :: SB.ByteString
someLaterTime = SB.pack "2012-07-05 09:00:01"

someOldTime = SB.pack "2012-07-05 08:59:59"

emptyBuf :: RingBuffer DummyState
emptyBuf = empty

someBuf = insertIntoBuf DS empty someTime

someLaterBuf = insertIntoBuf DS someBuf someLaterTime

reverseSomeLaterBuf = insertIntoBuf DS (insertIntoBuf DS empty someLaterTime) someTime

justLaterBuf = insertIntoBuf DS empty someLaterTime

tests :: Test
tests = test [ "Insert into empty buffer" ~: size (insertIntoBuf DS empty someTime) @?= 1
              , "Empty buffer is empty" ~: size empty @?= 0
              , "Insert same timestamp twice combines" ~: size (insertIntoBuf DS someLaterBuf someTime) @?= 2
              , "Insert newer timestamps extends" ~: size someLaterBuf @?= 2
              , "Insert order does not matter" ~: reverseSomeLaterBuf @?= someLaterBuf
              , "downSizeBuf is noop when buf too small" ~: downSizeBuf emptyBuf 1 @?= (emptyBuf, Nothing)
              , "downSizeBuf is noop when buf at limit" ~: downSizeBuf someBuf 1 @?= (someBuf, Nothing)
              , "downSizeBuf pops when too big" ~: downSizeBuf someBuf 0 @?= (empty, Just (someTime, DS))
              , "downSizeBuf pops oldest when too big" ~: downSizeBuf someLaterBuf 1 @?= (justLaterBuf, Just (someTime, DS))
              , "processLine inserts if too small" ~: processLine 2 (dummyMetric B.empty) someTime empty @?= (someBuf, Nothing)
              , "processLine drops if very old and at size" ~: processLine 1 (dummyMetric B.empty) someOldTime someBuf @?= (someBuf, Nothing)
              , "processLine downsizes if too big" ~: processLine 1 (dummyMetric B.empty) someLaterTime someBuf @?= (justLaterBuf, Just (someTime, DS))
              , "processLine should merge ABA style input" ~: processLine 2 (dummyMetric B.empty) someTime someLaterBuf @?= (someLaterBuf, Nothing)
              , "Out of order insertion should combine" ~: size (foldl (insertIntoBuf DS) empty (rights $ map getTime dummyInput)) @?= 4
              ]

main = do
  counts <- runTestTT tests
  let exitCode = case (errors counts) + (failures counts) of
        0 -> ExitSuccess
        a -> ExitFailure a
  exitWith exitCode
