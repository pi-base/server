import {
  Branch,
  BranchAccess,
  BranchName,
  Property,
  Sha,
  Space,
  Theorem,
  Token,
  Trait,
  User
} from './types'

import Api from './graph/Client/Api'
import { MASTER } from './constants'
import { ThunkAction } from 'redux-thunk'
import { Viewer as ViewerResponse } from './types/graph/Viewer'
import { State } from './reducers'

type Viewer = ViewerResponse['viewer']
type CheckProofInput = { theorem: Theorem } | { trait: Trait } | {}

export type AddProperty = { type: 'ADD_PROPERTY', property: Property }
export type AddSpace = { type: 'ADD_SPACE', space: Space }
export type AssertTheorem = { type: 'ASSERT_THEOREM', theorem: Theorem }
export type AssertTrait = { type: 'ASSERT_TRAIT', trait: Trait }
export type CheckProofs = { type: 'CHECK_PROOFS', trait?: Trait, theorem?: Theorem }
export type ChangeBranch = { type: 'CHANGE_BRANCH', branch: BranchName }
export type ChangeServer = { type: 'CHANGE_SERVER', host: string }
export type LoadViewer = { type: 'LOAD_VIEWER', viewer: Viewer, reset: boolean }
export type Login = { type: 'LOGIN', token: Token, user: User, branches: Branch[] }
export type LoginStarted = { type: 'LOGIN_STARTED', returnTo: string }
export type Logout = { type: 'LOGOUT' }
export type Search = { type: 'SEARCH', text?: string, formula?: string }
export type UpdateBranch = { type: 'UPDATE_BRANCH', branch: BranchName, sha: Sha }
export type ToggleDebug = { type: 'TOGGLE_DEBUG', on?: boolean }
export type SubmittingBranch = { type: 'SUBMITTING_BRANCH', branch: Branch }
export type SubmittedBranch = { type: 'SUBMITTED_BRANCH', branch: Branch, url: string }

export type Action
  = AddProperty
  | AddSpace
  | AssertTheorem
  | AssertTrait
  | ChangeBranch
  | ChangeServer
  | CheckProofs
  | LoadViewer
  | Login
  | LoginStarted
  | Logout
  | Search
  | UpdateBranch
  | ToggleDebug
  | SubmittingBranch
  | SubmittedBranch

type New<T> = Exclude<T, 'uid'>

type Async<R> = ThunkAction<
  Promise<R>,
  State,
  { graph: Api },
  Action
>

// Action creators

export const toggleDebug = (on: boolean | undefined): Action => ({ type: 'TOGGLE_DEBUG', on })

export const clearCache = (): Async<void> =>
  async _ => {
    if (typeof localStorage !== 'undefined') {
      localStorage.removeItem('piBase.reduxState')
    }
  }

const load = (viewer: Viewer, reset: boolean = false): Action => ({ type: 'LOAD_VIEWER', viewer, reset })

export const addProperty = (property: Property): Action => ({
  type: 'ADD_PROPERTY', property
})

export const addSpace = (space: Space): Action => ({
  type: 'ADD_SPACE', space
})

export const addTheorem = (theorem: Theorem): Action => ({
  type: 'ASSERT_THEOREM', theorem
})

export const addTrait = (trait: Trait): Action => ({
  type: 'ASSERT_TRAIT', trait
})

export const checkProofs = (input: CheckProofInput = {}): Action => ({
  type: 'CHECK_PROOFS', ...input
})

export const search = ({ text, formula }: { text?: string, formula?: string }): Action => {
  return { type: 'SEARCH', text, formula }
}

const submittingBranch = (branch: Branch): Action => ({
  type: 'SUBMITTING_BRANCH', branch
})
const submittedBranch = ({ branch, url }: { branch: Branch, url: string }): Action => ({
  type: 'SUBMITTED_BRANCH', branch, url
})

const fetchViewer = (): Async<void> =>
  async (dispatch, _, { graph }) => {
    return graph.viewer().then(viewer => {
      dispatch(load(viewer, true))
      dispatch(checkProofs())
    })
  }

export const boot = () => fetchViewer()

export const changeServer = (host: string): Async<void> =>
  async dispatch => {
    await dispatch(clearCache())
    dispatch({ type: 'CHANGE_SERVER', host })
    dispatch(fetchViewer())
  }

export const changeBranch = (branch: BranchName | undefined): Async<void> =>
  async (dispatch, _, { graph }) => {
    branch = branch || MASTER
    graph.branch = branch
    dispatch({ type: 'CHANGE_BRANCH', branch })
    dispatch(fetchViewer())
  }

export const loginUrl = (window: Window) =>
  (_dispatch, _state, { graph }) => graph.loginUrl({
    returnUrl: `${window.location.protocol}//${window.location.host}`
  })

export const startLogin = (window: Window): Async<void> =>
  async (dispatch, state, { graph }) => {
    dispatch({ type: 'LOGIN_STARTED', returnTo: window.location.pathname })
    window.location.href = loginUrl(window)(dispatch, state, { graph })
  }

export const login = (token: Token): Async<{ user: User, returnTo: string }> =>
  async (dispatch, getState, { graph }) => {
    graph.login(token)
    return graph.me().then(me => {
      const user = { name: me.name }

      const action: Login = {
        type: 'LOGIN',
        token,
        user,
        branches: me.branches.map(b => ({
          name: b.name,
          sha: b.sha,
          access: (b.access === 'admin' ? 'admin' : 'read' as BranchAccess),
          active: false,
          submitting: false,
          pullRequestUrl: undefined
        }))
      }
      dispatch(action)

      return {
        user,
        returnTo: getState().client.returnTo || '/'
      }
    })
  }

export const logout = (): Async<void> =>
  async (dispatch, _, { graph }) => {
    graph.logout()
    dispatch({ type: 'LOGOUT' })
    return
  }

export const resetBranch = (branch: BranchName, to: Sha): Async<void> =>
  async (dispatch, _, { graph }) =>
    graph.resetBranch(branch, to).then(_ => {
      dispatch(fetchViewer())
    })

export const submitBranch = (branch: Branch): Async<void> =>
  async (dispatch, _, { graph }) => {
    dispatch(submittingBranch(branch))
    graph.submitBranch(branch.name).then(submitted =>
      dispatch(submittedBranch({ branch, url: submitted.url }))
    )
  }

export const assertTrait = (trait: Trait): Async<Trait> =>
  async (dispatch, _, { graph }) =>
    graph.assertTrait(trait).then(result => {
      dispatch(addTrait(result))
      dispatch(checkProofs({ trait: result }))
      return result
    })

export const assertTheorem = (theorem: New<Theorem>): Async<Theorem> =>
  async (dispatch, _, { graph }) =>
    graph.assertTheorem(theorem).then(result => {
      dispatch(addTheorem(result))
      dispatch(checkProofs({ theorem: result }))
      return result
    })

export const createSpace = (space: New<Space>): Async<Space> =>
  async (dispatch, _, { graph }) =>
    graph.createSpace(space).then(result => {
      dispatch(addSpace(result))
      return result
    })

export const createProperty = (property: New<Property>): Async<Property> =>
  async (dispatch, _, { graph }) =>
    graph.createProperty(property).then(result => {
      dispatch(addProperty(result))
      return result
    })

export const updateSpace = (space: Space): Async<Space> =>
  async (dispatch, _, { graph }) =>
    graph.updateSpace(space).then(result => {
      dispatch(addSpace(result))
      return result
    })

export const updateProperty = (property: Property): Async<Property> =>
  async (dispatch, _, { graph }) =>
    graph.updateProperty(property).then(result => {
      dispatch(addProperty(result))
      return result
    })

export const updateTrait = (trait: Trait): Async<Trait> =>
  async (dispatch, _, { graph }) =>
    graph.updateTrait(trait).then(result => {
      dispatch(addTrait(result))
      return result
    })

export const updateTheorem = (theorem: Theorem): Async<Theorem> =>
  async (dispatch, _, { graph }) =>
    graph.updateTheorem(theorem).then(result => {
      dispatch(addTheorem(result))
      return result
    })
