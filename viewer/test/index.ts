import { Store } from 'redux'
import { State, makeReducer } from '../src/reducers'
import { Action } from '../src/actions'
import TestClient from './Client'

import configureStore from 'redux-mock-store'

import fs from 'fs'

export const noop = _ => { }

const mockStore = configureStore([])

const reducer = makeReducer(new TestClient())

export const makeStore = (state: Partial<State>): Store<State> => {
  // Run through the root reducer once to make sure the state
  // is well-structured
  const initialState = reducer(state as State, {} as Action)
  return mockStore(initialState)
}

let _fixtureStore: Store<State> | undefined = undefined

export const fixtureStore = (): Store<State> => {
  if (!_fixtureStore) {
    const viewer = JSON.parse(fs.readFileSync('./test/fixtures/viewer.json').toString()).viewer
    const state = reducer(undefined, {
      type: 'LOAD_VIEWER',
      viewer,
      reset: true
    })
    _fixtureStore = mockStore(state)
  }
  return _fixtureStore!
}
