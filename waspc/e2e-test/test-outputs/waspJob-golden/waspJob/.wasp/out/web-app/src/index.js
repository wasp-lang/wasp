import React from 'react'
import ReactDOM from 'react-dom'
import { ReactQueryCacheProvider } from 'react-query'

import router from './router'
import queryCache from './queryCache'
import * as serviceWorker from './serviceWorker'

import './index.css'


ReactDOM.render(
  <ReactQueryCacheProvider queryCache={queryCache}>
    { router }
  </ReactQueryCacheProvider>,
  document.getElementById('root')
)

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
