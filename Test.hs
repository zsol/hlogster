import Buffer
import Metric
import Test.HUnit
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy.Char8 as B

data DummyState = DS deriving (Show, Eq)

instance IMetricState DummyState where
  combine _ _ = DS
  toResults a _ = [("", "", a)]

dummyMetric :: a -> DummyState
dummyMetric _ = DS

dummyInput = map B.pack [ "2012-07-05 09:00:00,759"
                        , "2012-07-05 09:00:01,764"
                        , "2012-07-05 09:00:02,765"
                        , "2012-07-05 09:00:03,765"
                        , "2012-07-05 09:00:04,771"
                        , "2012-07-05 09:00:05,773"
                        , "2012-07-05 09:00:01,776"
                        , "2012-07-05 09:00:01,776"
                        , "2012-07-05 09:00:02,783"
                        , "2012-07-05 09:00:06,784"
                        , "2012-07-05 09:00:00,784"
                        , "2012-07-05 09:00:06,795"
                        , "2012-07-05 09:00:06,796"
                        , "2012-07-05 09:00:06,800"
                        , "2012-07-05 09:00:00,801"
                        , "2012-07-05 09:00:00,801"
                        , "2012-07-05 09:00:00,816"
                        , "2012-07-05 09:00:00,816"
                        , "2012-07-05 09:00:00,817"
                        , "2012-07-05 09:00:00,818"
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
              , "Insert same timestamp twice combines" ~: size (insertIntoBuf DS someBuf someTime) @?= 1
              , "Insert newer timestamps extends" ~: size someLaterBuf @?= 2
              , "Insert order does not matter" ~: reverseSomeLaterBuf @?= someLaterBuf
              , "downSizeBuf is noop when buf too small" ~: downSizeBuf emptyBuf 1 @?= (emptyBuf, Nothing)
              , "downSizeBuf is noop when buf at limit" ~: downSizeBuf someBuf 1 @?= (someBuf, Nothing)
              , "downSizeBuf pops when too big" ~: downSizeBuf someBuf 0 @?= (empty, Just (someTime, DS))
              , "downSizeBuf pops oldest when too big" ~: downSizeBuf someLaterBuf 1 @?= (justLaterBuf, Just (someTime, DS))
              , "processLine inserts if too small" ~: processLine 2 dummyMetric B.empty someTime empty @?= (someBuf, Nothing)
              , "processLine drops if very old and at size" ~: processLine 1 dummyMetric B.empty someOldTime someBuf @?= (someBuf, Nothing)
              , "processLine downsizes if too big" ~: processLine 1 dummyMetric B.empty someLaterTime someBuf @?= (justLaterBuf, Just (someTime, DS))
              ]