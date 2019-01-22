import { Action } from '../actions'
import Api from '../graph/Client/Api'
import { Token } from '../types'
import { GRAPH_URL, MASTER } from '../constants'

export type State = {
  branch: string
  host: string
  token: Token | null
  returnTo: string | null
}

export const initial: State = {
  branch: MASTER,
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
      case 'CHANGE_BRANCH':
        api.branch = action.branch
        return { ...state, branch: action.branch }
      case 'CHANGE_SERVER':
        api.host = action.host
        return { ...state, host: action.host }
      case 'LOGIN':
        api.login(action.token)
        return { ...state, token: action.token, returnTo: null }
    }

    return state
  }
