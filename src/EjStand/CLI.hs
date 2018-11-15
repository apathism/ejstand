{-# LANGUAGE RecordWildCards #-}
module EjStand.CLI
  ( ejStand
  )
where

import           Control.Applicative           (liftA2)
import           Data.Maybe                    (fromMaybe)
import           Data.Text                     (Text)
import           EjStand                       (getVersion)
import           EjStand.Models.Standing       (GlobalConfiguration (..), defaultGlobalConfiguration)
import           EjStand.Parsers.Configuration (retrieveGlobalConfiguration)
import           EjStand.Web.Server            (ejStandWebServer)
import           Options.Applicative

data CLIArguments = CLIArguments { customConfig :: !(Maybe FilePath)
                                 , hostname     :: !(Maybe Text)
                                 , port         :: !(Maybe Int)
                                 }
                    deriving (Show)

versionString :: String
versionString = "EjStand " ++ getVersion

defaultGlobalConfigurationPaths :: [FilePath]
defaultGlobalConfigurationPaths = ["/etc/ejstand.cfg", "/etc/ejstand/ejstand.cfg", "./ejstand.cfg"]

parseCLIArguments :: Parser CLIArguments
parseCLIArguments =
  CLIArguments
    <$>  option
           (Just <$> str)
           (long "config" <> short 'c' <> metavar "FILE" <> help "Path to main configururation file" <> value Nothing)
    <*>  option (Just <$> str)
                (long "hostname" <> short 'h' <> metavar "HOSTNAME" <> help "Hostname to listen to" <> value Nothing)
    <*> option (Just <$> auto) (long "port" <> short 'p' <> metavar "PORT" <> help "Port to listen to" <> value Nothing)
    <**> abortOption (InfoMsg versionString) (long "version" <> help "Show version information" <> hidden)
    <**> abortOption ShowHelpText            (long "help" <> help "Show this help text" <> hidden)

getGlobalConfigurationByCLIArguments :: CLIArguments -> IO GlobalConfiguration
getGlobalConfigurationByCLIArguments CLIArguments {..} =
  let (paths, onError) = case customConfig of
        (Just path) -> ([path], fail $ "Unable to read EjStand configuration from " ++ path)
        Nothing     -> (defaultGlobalConfigurationPaths, return defaultGlobalConfiguration)
  in  liftA2 fromMaybe onError (retrieveGlobalConfiguration paths)

ejStand :: IO ()
ejStand = do
  let cliParserOpts = info parseCLIArguments $ fullDesc <> header
        "EjStand: Ejudge Configurable Web Standings Daemon for Multiple Contests"
  cliArguments <- execParser cliParserOpts
  globalCfg    <- getGlobalConfigurationByCLIArguments cliArguments
  let updatedGlobalCfg = globalCfg { ejStandHostname = fromMaybe (ejStandHostname globalCfg) (hostname cliArguments)
                                   , ejStandPort     = fromMaybe (ejStandPort globalCfg) (port cliArguments)
                                   }
  ejStandWebServer updatedGlobalCfg
