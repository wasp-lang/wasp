import waspLogo from './waspLogo.png'
import './Main.css'
import { useState } from 'react'
import startGeneratingNewApp from '@wasp/actions/startGeneratingNewApp'
import getAppGenerationResult from '@wasp/queries/getAppGenerationResult'
import { useQuery } from '@wasp/queries'

const MainPage = () => {
  const [appName, setAppName] = useState('')
  const [appDesc, setAppDesc] = useState('')
  const [appId, setAppId] = useState('')
  const [generationDone, setGenerationDone] = useState(false)
  const { data: appGenerationResult } = useQuery(
    getAppGenerationResult,
    { appId },
    { enabled: !!appId && !generationDone, refetchInterval: 3000 }
  )

  if (appGenerationResult?.status === 'success' || appGenerationResult?.status === 'failure') {
    if (!generationDone) {
      setGenerationDone(true)
    }
  }

  const logs = appGenerationResult?.messages.filter(m => m.type === 'log').map(m => m.text)
  let files = {}
  {
    appGenerationResult?.messages.filter(m => m.type === 'write-file').map(m => m.text.split('\n')).forEach(([path, ...contentLines]) => {
      files[path] = contentLines.join('\n')
    })
  }

  function fillInExampleAppDetails () {
    setAppName('TodoApp')
    setAppDesc('A simple todo app with one main page that lists all the tasks. I can create new tasks, or toggle existing ones.'
     + 'User owns tasks. User can only see and edit their own tasks. Tasks are saved in the database.')
  }

  async function startGenerating () {
    if (!(appName && appDesc)) {
      return window.alert('Please enter an app name and description.')
    }
    setAppId(await startGeneratingNewApp({ appName, appDesc }))
  }

  return (
    <div className="container">
      <main>
        <div className="logo">
          <img src={waspLogo} alt="wasp" />
        </div>
        <p>
          Wasp AI App Generator
        </p>
        
        <div>
          <input
            type="text"
            placeholder="AppName"
            value={appName}
            onChange={(e) => setAppName(e.target.value)}
            disabled={appId}
          />
        </div>

        <div>
          <textarea
            placeholder="App description"
            value={appDesc}
            rows="5"
            cols="50"
            onChange={(e) => setAppDesc(e.target.value)}
            disabled={appId}
          />
        </div>

        <button disabled={appId} onClick={() => fillInExampleAppDetails()}>Fill in with example app details</button>
        <button onClick={() => startGenerating()}>Generate</button>

        { appId && !generationDone && (
          <div> Generating... </div>
        )}

        <div key="logs">
          { logs && logs.map((log, i) =>
            <div style={{"backgroundColor": "yellow"}} key={i}>
              {log}
            </div>
            )  }
        </div>

        <div key="files">
          { files && Object.keys(files).map((path) =>
            <div key={path}>
              <div> {path}: </div>
              <div style={{"backgroundColor": "grey"}}> {files[path]} </div>
            </div>
            )  }
        </div>
      </main>
    </div>
  )
}
export default MainPage
