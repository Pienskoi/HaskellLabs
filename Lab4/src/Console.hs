module Console
    ( parseOptions,
      defaultOptions,
      Command(..),
      Options(..),
    ) where

data Command = Command { commandName :: String, arguments :: [String] }
    deriving Show

data Options = Options { command :: Command, logname :: Maybe String, silent :: Bool, html :: Maybe String, help :: Bool }
    deriving Show

defaultOptions :: Options
defaultOptions = Options { command = Command { commandName = "", arguments = [] }, logname = Nothing, silent = False, html = Nothing, help = False }

parseOptions :: [String] -> Options -> Options
parseOptions [] opts = opts
parseOptions ("-c":c:args) opts = parseOptions args opts { command = Command { commandName = c, arguments = parseCommand args } }
parseOptions ("--command":c:args) opts = parseOptions args opts { command = Command { commandName = c, arguments = parseCommand args } }
parseOptions ("-l":l:args) opts = parseOptions args opts { logname = Just l }
parseOptions ("--log":l:args) opts = parseOptions args opts { logname = Just l }
parseOptions ("-s":args) opts = parseOptions args opts { silent = True }
parseOptions ("--silent":args) opts = parseOptions args opts { silent = True }
parseOptions ("--html":h:args) opts = parseOptions args opts { html = Just h }
parseOptions ("-h":args) opts = parseOptions args opts { help = True }
parseOptions ("--help":args) opts = parseOptions args opts { help = True }
parseOptions (_:args) opts = parseOptions args opts

parseCommand :: [String] -> [String]
parseCommand [] = []
parseCommand ("-l":_) = []
parseCommand ("--log":_) = []
parseCommand ("-s":_) = []
parseCommand ("--silent":_) = []
parseCommand ("--html":_) = []
parseCommand ("-h":_) = []
parseCommand ("--help":_) = []
parseCommand (arg:args) = arg : parseCommand args
