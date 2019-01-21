import { Store, applyMiddleware, compose, createStore } from 'redux'
import { State, makeReducer } from './reducers'
import thunk, { ThunkMiddleware } from 'redux-thunk'
import Api from './graph/Client/Api'

import { AnyAction } from 'redux'
import { createLogger } from 'redux-logger'

const inDevelopment = process.env.NODE_ENV === 'development'

// tslint:disable-next-line no-any
const composeEnhancers = (window as any).__REDUX_DEVTOOLS_EXTENSION_COMPOSE__ || compose

type MiddlewareOpts = {
  graph: Api
  storage?: Storage
}

const makeMiddleware = ({ graph }: MiddlewareOpts) => {
  const middleware = [
    thunk.withExtraArgument({ graph }) as ThunkMiddleware<State, AnyAction>
  ]

  if (inDevelopment) {
    middleware.push(createLogger({ collapsed: true }))
  }

  // TODO: rollbar middleware?

  return middleware
}

const persistState = ({ storage, graph }: { storage: Storage, graph: Api }) => next => (reducer, _) => {
  let load, dump

  if (inDevelopment) {
    dump = state => state
    load = json => {
      const traits = new Map()
      json.traits.forEach(([sid, ts]) => {
        const map = ts.reduce(
          (acc, [pid, trait]) => acc.set(pid, trait),
          new Map()
        )
        traits.set(sid, map)
      })

      return {
        ...json,
        traits,
        proofs: new Map(json.proofs),
        properties: new Map(json.properties),
        spaces: new Map(json.spaces),
        theorems: new Map(json.theorems),
        version: json.version && {
          active: json.version.active,
          branches: new Map(json.version.branches)
        }
      }
    }
  } else {
    dump = (state: State) => ({ version: state.version })
    load = json => {
      return {
        version: json.version && {
          active: json.version.active,
          branches: new Map(json.version.branches)
        }
      }
    }
  }

  const loaded = storage.getItem('piBase.reduxState')

  let initialState: State | undefined
  if (loaded) {
    try {
      initialState = load(JSON.parse(loaded))
      if (initialState && initialState.client) {
        graph.host = initialState.client.host
      }
      if (initialState && initialState.client.token) {
        graph.login(initialState.client.token)
      }
    } catch (e) {
      console.error('Failed to load state from storage:', e)
    }
  }
  const store = next(reducer, initialState)

  store.subscribe(() => {
    const state = store.getState()

    storage.setItem('piBase.reduxState', JSON.stringify(dump(state)))
  })

  return store
}

export function makeStore({ graph, storage = localStorage }: MiddlewareOpts): Store<State> {
  const reducer = makeReducer(graph)

  const middleware = makeMiddleware({ graph })

  return createStore(
    reducer,
    composeEnhancers(
      applyMiddleware(...middleware),
      persistState({ storage, graph })
    )
  )
}