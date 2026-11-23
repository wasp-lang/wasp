module Wasp.Cli.Command.Db.Studio
  ( studio,
    StudioArgs (..),
    studioArgsParser,
  )
where

import Network.Socket (PortNumber)
import qualified Options.Applicative as Opt
import StrongPath ((</>))
import Wasp.Cli.Command (Command, require)
import Wasp.Cli.Command.Db (makeDbCommand)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.Cli.Port (resolvePort)
import Wasp.Cli.Util.Parser (ArgsParser (..))
import Wasp.Cli.Util.PortArgument (portParser)
import Wasp.Generator.DbGenerator.Jobs (runStudio)
import qualified Wasp.Generator.ServerGenerator.RunConfig as ServerGenerator
import Wasp.Generator.Setup (SetupGoal (PrismaCliReady))
import qualified Wasp.Job as Job
import qualified Wasp.Message as Msg
import Wasp.Project.Common (generatedAppDirInWaspProjectDir)

newtype StudioArgs = StudioArgs
  { port :: Maybe PortNumber
  }

studioArgsParser :: ArgsParser StudioArgs
studioArgsParser =
  ArgsParser "wasp db studio" $
    StudioArgs
      <$> Opt.optional (portParser "port" ("Port to run the database studio on (default: " ++ show defaultPrismaStudioPort ++ ")"))

studio :: StudioArgs -> Command ()
studio args = makeDbCommand [PrismaCliReady] $ \_appSpec projectRunConfig -> do
  InWaspProject waspProjectDir <- require
  ValidNodeAndNpm <- require
  let genProjectDir = waspProjectDir </> generatedAppDirInWaspProjectDir

  studioPort <-
    resolvePort
      args.port
      defaultPrismaStudioPort
      []
      "Choose a different port with --port, or free up this one."
      "Free at least one of those ports by exiting the program listening on it, or choose the port yourself with --port."

  cliSendMessageC $ Msg.Start "Running studio..."

  _ <- Job.run $ Job.prefixWith Job.Db $ runStudio (ServerGenerator.makeEnvVars projectRunConfig) genProjectDir studioPort

  error "This should never happen, studio should never stop."

-- | The port Prisma Studio uses when it isn't given one.
defaultPrismaStudioPort :: PortNumber
defaultPrismaStudioPort = 5555
