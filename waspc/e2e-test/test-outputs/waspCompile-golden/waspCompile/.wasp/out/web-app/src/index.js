import React from 'react'
import ReactDOM from 'react-dom'
import { QueryClientProvider } from 'react-query'

import router from './router'
import { queryClient } from './queryClient'
import * as serviceWorker from './serviceWorker'

import './index.css'


ReactDOM.render(
  <QueryClientProvider client={queryClient}>
    { router }
  </QueryClientProvider>,
  document.getElementById('root')
)

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
