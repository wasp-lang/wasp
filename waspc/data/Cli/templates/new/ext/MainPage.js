import React from 'react'
import waspLogo from './waspLogo.png'
import './Main.css'

const MainPage = () => {
  return (
    <div className="container">
      <main>
        <div className="logo">
          <img src={waspLogo} alt="wasp" />
        </div>

        <h2 className="welcome-title"> Welcome to Wasp - you just started a new app! </h2>
        <h3 className="welcome-subtitle">
          This is page <code>MainPage</code> located at route <code>/</code>.
          Open <code>ext/MainPage.js</code> to edit it.
        </h3>

        <div className="buttons">
          <a
            className="button button-filled"
            href="https://wasp-lang.dev/docs/tutorials/todo-app"
            target="_blank"
            rel="noreferrer noopener"
          >
            Take the Tutorial
          </a>
          <a
            className="button button-outline"
            href="https://discord.com/invite/rzdnErX"
            target="_blank"
            rel="noreferrer noopener"
          >
            Chat on Discord
          </a>
        </div>
      </main>
    </div>
  )
}
export default MainPage
