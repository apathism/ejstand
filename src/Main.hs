--import EjStand.DataParser
--import Text.Printf (printf)
import           EjStand.ConfigParser

main :: IO ()
main = do
  cfg <- retrieveGlobalConfiguration 
  print $ show cfg