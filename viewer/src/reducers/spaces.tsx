import { Action } from '../actions'
import { Citation, Id, Space } from '../types'

export type State = Map<Id, Space>

export const reducer = (
  state: State | undefined,
  action: Action
): State => {
  state = state || new Map()

  switch (action.type) {
    case 'ADD_SPACE':
      return state.set(action.space.uid, action.space)

    case 'CHANGE_BRANCH':
      return new Map()

    case 'LOAD_VIEWER':
      const next = new Map(action.reset ? [] : state)
      action.viewer.spaces.forEach(s => {
        next.set(s.uid, {
          uid: s.uid,
          name: s.name,
          aliases: s.aliases,
          description: s.description,
          references: s.references as Citation[] // TODO: check `type` against enum
        })
      })
      return new Map(next)

    default:
      return state
  }
}

export default reducer