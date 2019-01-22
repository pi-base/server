import { Action } from '../actions'
import Api from '../graph/Client/Api'
import { Token } from '../types'
import { GRAPH_URL } from '../constants'

export type State = {
  host: string
  token: Token | null
  returnTo: string | null
}

export const initial: State = {
  host: GRAPH_URL,
  token: null,
  returnTo: null
}

export const makeReducer = (api: Api) =>
  (
    state: State | undefined,
    action: Action
  ): State => {
    state = state || initial

    switch (action.type) {
      case 'CHANGE_SERVER':
        api.host = action.host
        return { ...state, host: action.host }
      case 'LOGIN':
        api.login(action.token)
        return { ...state, token: action.token, returnTo: null }
    }

    return state
  }
