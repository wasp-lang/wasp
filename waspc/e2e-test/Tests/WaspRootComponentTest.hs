module Tests.WaspRootComponentTest (waspRootComponent) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( cdIntoCurrentProject,
    createFile,
    insertLineAtWaspFile,
    waspCliCompile,
    waspCliNew,
  )

waspRootComponent :: GoldenTest
waspRootComponent = do
  let rootComponentContent =
        unlines
          [ "export default function App({ children }) {",
            "  return (",
            "    <div className=\"app\">",
            "      <h1>Wasp</h1>",
            "      {children}",
            "    </div>",
            "  );",
            "}"
          ]

  makeGoldenTest "waspRootComponent" $
    sequence
      [ waspCliNew,
        cdIntoCurrentProject,
        createFile rootComponentContent "./src/client" "App.jsx",
        insertLineAtWaspFile "  client: { rootComponent: import App from \"@client/App.jsx\" }," 2,
        waspCliCompile
      ]
