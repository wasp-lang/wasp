import React from 'react'
import ReactDOM from 'react-dom/client'
import { WaspApp } from 'wasp/client/app'

ReactDOM.createRoot(document.getElementById('root') as HTMLElement).render(
  <React.StrictMode>
    <WaspApp />
  </React.StrictMode>
)
