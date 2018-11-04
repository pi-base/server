import 'babel-polyfill'
import './index.css'
import './errors'

import * as React from 'react'
import * as ReactDOM from 'react-dom'

import { makeStore } from './store'

import makeApp from './components/App'
import Client from './graph/Client'

const graph = new Client()

const store = makeStore({ graph })

const App = makeApp({
  graph,
  store,
  window
})

ReactDOM.render(
  <App />,
  document.getElementById('root')
)
