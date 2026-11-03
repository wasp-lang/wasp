-- | Ports of the tests that run Wasp apps. Each test gets its own ports so the
-- tests can run in parallel. Keeping them all in one place avoids collisions.
module E2e.Ports
  ( AppPorts (..),
    appPortList,
    appPortArgs,
    waspStartUncompiledProject,
    waspStartCompiledProject,
    waspStartWebAppCrash,
    waspStartSigterm,
    waspStartSighup,
    waspStartTerminalHangup,
    waspBuildStart,
    waspStudioPort,
    prismaStudioPort,
  )
where

data AppPorts = AppPorts
  { clientPort :: Int,
    serverPort :: Int
  }

appPortList :: AppPorts -> [Int]
appPortList ports = [ports.clientPort, ports.serverPort]

-- | For `wasp start` and `wasp build start`.
appPortArgs :: AppPorts -> [String]
appPortArgs ports = ["--client-port", show ports.clientPort, "--server-port", show ports.serverPort]

waspStartUncompiledProject, waspStartCompiledProject, waspStartWebAppCrash, waspStartSigterm, waspStartSighup, waspStartTerminalHangup :: AppPorts
waspStartUncompiledProject = AppPorts 31000 31001
waspStartCompiledProject = AppPorts 31010 31011
waspStartWebAppCrash = AppPorts 31020 31021
waspStartSigterm = AppPorts 31030 31031
waspStartSighup = AppPorts 31040 31041
waspStartTerminalHangup = AppPorts 31050 31051

waspBuildStart :: AppPorts
waspBuildStart = AppPorts 31060 31061

-- | Fixed by Wasp. Only one test may use each of them.
waspStudioPort, prismaStudioPort :: Int
waspStudioPort = 4000
prismaStudioPort = 5555
