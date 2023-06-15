import waspLogo from './waspLogo.png'
import './Main.css'
import { useState } from 'react'

const MainPage = () => {
  const [appName, setAppName] = useState('')
  const [appDesc, setAppDesc] = useState('')
  const [inputDisabled, setInputDisabled] = useState(false)

  const fillInExampleAppDetails = () => {
    setAppName('TodoApp')
    setAppDesc('A simple todo app with one main page that lists all the tasks. I can create new tasks, or toggle existing ones.'
     + 'User owns tasks. User can only see and edit their own tasks. Tasks are saved in the database.')
  }

  const generate = () => {
    if (!(appName && appDesc)) {
      return window.alert('Please enter an app name and description.')
    }
    setInputDisabled(true)
    window.alert('Generating app. This may take a while. Please wait.')
    // Idea #1:
    //   We could use websockets -> we first send the message to start generating the app, then we pick up messages from the server.
    //   We would need to identify this specific client probably, so that the server knows whom to send messages to.
    //   We could do that by sending some unique UUID we generate on client for new request, and server would send that UUID back.
    //   But wait, then all the clients can see those messages coming?
    //   I think we need to be able to open a websocket room that only our client and server see.
    // Idea #2:
    //   We use action to start the generation, and it runs as long as generation runs.
    //   It stores the results of generation into the local variable, under the UUID specific for this generation.
    //   We can then use another action, that we call regularly (every second) from client, to get the results of generation, and we can use the UUID to get the results.
    //   It would need to be smart about how to correctly consume piece of the results, but I think we can handle that.
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
            disabled={inputDisabled}
          />
        </div>

        <div>
          <textarea
            placeholder="App description"
            value={appDesc}
            rows="5"
            cols="50"
            onChange={(e) => setAppDesc(e.target.value)}
            disabled={inputDisabled}
          />
        </div>

        <button disabled={inputDisabled} onClick={() => fillInExampleAppDetails()}>Fill in with example app details</button>
        <button onClick={() => generate()}>Generate</button>
      </main>
    </div>
  )
}
export default MainPage
