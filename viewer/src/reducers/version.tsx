import { Branch, BranchState } from '../types'

import { Action } from '../actions'
import { MASTER } from '../constants'

export type State = {
  active: Branch
  branches: Map<Branch, BranchState>
}

export const initial = {
  active: MASTER,
  branches: new Map()
}

const updateBranch = (state, name, updates) => {
  const branches = new Map(state.branches)
  let branch = branches.get(name)
  if (branch) {
    branches.set(name, { ...branch, ...updates })
  }
  return { ...state, branches }
}

export const reducer = (
  state: State | undefined,
  action: Action
): State => {
  state = state || initial as State

  let branches

  switch (action.type) {
    case 'CHANGE_BRANCH':
      return { ...state, active: action.branch }
    case 'UPDATE_BRANCH':
      return updateBranch(state, action.branch, { sha: action.sha })
    case 'LOGIN':
      branches = new Map()
      action.branches.forEach(b => branches.set(b.name, b))
      // TODO: this should check that the SHA matches the loaded data
      let active = state.active || action.branches.find(b => b.access === 'read')!.name
      return { ...state, branches, active }
    case 'LOGOUT':
      return initial
    case 'SUBMITTING_BRANCH':
      return updateBranch(state, action.branch, {
        submitting: true
      })
    case 'SUBMITTED_BRANCH':
      return updateBranch(state, action.branch, {
        submitting: false,
        pullRequestUrl: action.url
      })
    default:
      return state
  }
}

export default reducer