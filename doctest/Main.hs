import qualified Test.DocTest as DocTest

main :: IO ()
main = do
    DocTest.doctest [ "--fast", "--preserve-it", "src" ]
