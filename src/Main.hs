{--
import EjStand.ConfigParser
import EjStand.StandingBuilder
import EjStand.HtmlRenderer
import Data.Text.Lazy.IO (writeFile)
import Prelude hiding (writeFile)

main :: IO ()
main = do
  globalConfig <- retrieveGlobalConfiguration
  [localConfig] <- retrieveStandingConfigs globalConfig
  standingSource <- prepareStandingSource  globalConfig localConfig
  let standing = buildStanding localConfig standingSource
  writeFile "/home/apathism/ejstand.html" $ renderStanding standing
--}

import           EjStand.WebApplication (runEjStand)

main :: IO ()
main = runEjStand
