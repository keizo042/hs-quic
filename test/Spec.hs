import           Network.QUIC
import           System.IO
import           Test.HUnit

tests = TestList $ testsCrypt
                ++ testsTimeFormat
                ++ testsPriority
                ++ testsFrame

testsCrypt =
    [
     "decode short Header" ~: testHeader1 ~?= actualHeader1
     "decode Long Header"  ~: testHeader2 ~?= actualHeader2
     ]

testsTimeFormat = []

testsPriority = []

testsFrame = []

main :: IO ()
main = runTestText (putTextToHandle stderr False) tests
