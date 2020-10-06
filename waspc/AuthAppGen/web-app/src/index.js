import React from 'react'
import ReactDOM from 'react-dom'
import { Provider } from 'react-redux'
import { ReactQueryCacheProvider } from 'react-query'

import router from './router'
import { configureStore } from './store'
import queryCache from './queryCache'
import { rootReducer } from './reducers'
import * as serviceWorker from './serviceWorker'

import './index.css'


const store = configureStore(rootReducer)

ReactDOM.render(
  <ReactQueryCacheProvider queryCache={queryCache}>
    <Provider store={store}>
      { router }
    </Provider>
  </ReactQueryCacheProvider>,
  document.getElementById('root')
)

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
