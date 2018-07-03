import Ejudge.Standings.DataParser
import Text.Printf (printf)

main :: IO()
main = do
  parsed <- sequence $ map (parseEjudgeXML . printf "xmls/%d.xml") ([5501..5525] :: [Int])
  print $ length $ show parsed
