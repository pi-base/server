import * as client from './reducers/client'
import * as properties from './reducers/properties'
import * as prover from './reducers/prover'
import * as spaces from './reducers/spaces'
import * as theorems from './reducers/theorems'
import * as traits from './reducers/traits'
import * as user from './reducers/user'
import * as version from './reducers/version'

import Api from './graph/Client/Api'

import { AnyAction, Reducer, combineReducers } from 'redux'
import { Action } from './actions'

export type State = {
  client: client.State
  debug: boolean
  proofs: prover.State
  properties: properties.State
  spaces: spaces.State
  theorems: theorems.State
  traits: traits.State
  user: user.State
  version: version.State
}

const debugReducer = (debug: boolean | undefined, action: any): boolean => {
  if (debug === undefined) { debug = process.env.NODE_ENV === 'development' }

  if (action.type === 'TOGGLE_DEBUG') {
    if (action.on === undefined) {
      return !debug // toggle
    } else {
      return action.on
    }
  }

  return debug
}

export const makeReducer = (api: Api): Reducer<State, Action> => {
  const clientReducer = client.makeReducer(api)

  const combined = combineReducers<State>({
    client: clientReducer,
    debug: debugReducer,
    proofs: s => s || prover.initial,
    properties: properties.reducer,
    spaces: spaces.reducer,
    traits: traits.reducer,
    theorems: theorems.reducer,
    user: user.reducer,
    version: version.reducer,
  })

  const reducer = (state: State | undefined, action: AnyAction): State => {
    let next = combined(state, action)
    return prover.reducer(next, action)
  }

  return reducer
}
