module Wasp.Cli.Command.Db.Studio
  ( studio,
    studioArgsParser,
  )
where

import StrongPath ((</>))
import Wasp.Cli.Command (Command, require)
import Wasp.Cli.Command.Db (makeDbCommand)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.Cli.DevDb.ArgumentsParser (DevDbOptions, devDbOptionsParser)
import Wasp.Cli.Util.Parser (ArgsParser (..))
import Wasp.Generator.DbGenerator.Jobs (runStudio)
import qualified Wasp.Generator.ServerGenerator.RunConfig as ServerGenerator
import Wasp.Generator.Setup (SetupGoal (PrismaCliReady))
import qualified Wasp.Job as Job
import qualified Wasp.Message as Msg
import Wasp.Project.Common (generatedAppDirInWaspProjectDir)

studio :: DevDbOptions -> Command ()
studio devDbOptions = makeDbCommand devDbOptions [PrismaCliReady] $ \_appSpec projectRunConfig -> do
  InWaspProject waspProjectDir <- require
  ValidNodeAndNpm <- require
  let genProjectDir = waspProjectDir </> generatedAppDirInWaspProjectDir

  cliSendMessageC $ Msg.Start "Running studio..."

  _ <- Job.run $ Job.prefixWith Job.Db $ runStudio (ServerGenerator.makeEnvVars projectRunConfig) genProjectDir

  error "This should never happen, studio should never stop."

studioArgsParser :: ArgsParser DevDbOptions
studioArgsParser = ArgsParser "wasp db studio" devDbOptionsParser
