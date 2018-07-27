import           EjStand.ConfigParser

main :: IO ()
main = do
  cfg <- retrieveGlobalConfiguration
  print $ show cfg
