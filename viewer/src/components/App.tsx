import * as React from 'react'

import { Route, Switch } from 'react-router'

import Api from '../graph/Client/Api'
import Debug from '../models/Debug'
import { BrowserRouter } from 'react-router-dom'
import GraphExplorer from '../components/GraphExplorer'
import Layout from './Layout'
import { Provider } from 'react-redux'
import { Store } from 'redux'
import routes from '../routes'

interface AppParams {
  graph: Api
  store: Store
  window?: Window
}

const makeApp = ({ graph, store, window }: AppParams) => () => {
  const layout = () => <Layout>{routes}</Layout>

  addWindowHooks({ graph, store, window })

  return (
    <Provider store={store}>
      <BrowserRouter>
        <Switch>
          {process.env.NODE_ENV === 'development'
            ? <Route path="/graph" exact={true} component={GraphExplorer(graph)} />
            : null
          }
          <Route component={layout} />
        </Switch>
      </BrowserRouter>
    </Provider>
  )
}

export default makeApp

declare global {
  interface Window {
    piBase: Debug
    // tslint:disable no-any
    $: any
    Rollbar: any
    // tslint:enable no-any
  }
}

const addWindowHooks = ({ store, graph, window }: AppParams) => {
  if (!window) { return }

  window.piBase = new Debug({ store, graph })

  const err = window.onerror

  window.onerror = (e) => {
    const m = window.$('#errorModal')

    m.find('.details').html(`<pre>${e}</pre>`);

    // tslint:disable-next-line no-any
    (m as any).modal('show') // added by bootstrap

    err(e)
  }
}