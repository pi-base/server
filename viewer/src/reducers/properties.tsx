import { Action, Id, Property } from '../types'

export type State = Map<Id, Property>

export const initial = new Map()

export const reducer = (state: State, action: Action): State => {
  state = state || initial
  let next

  switch (action.type) {
    case 'LOAD_VIEWER':
      next = new Map(state)
      action.viewer.viewer.properties.forEach(p => {
        next.set(p.uid, {
          uid: p.uid,
          name: p.name,
          aliases: p.aliases,
          description: p.description,
          references: p.references
        })
      })
      return next

    case 'CHANGE_BRANCH':
      return new Map()

    case 'ADD_PROPERTY':
      next = new Map(state)
      next.set(action.property.uid, action.property)
      return next

    default:
      return state
  }
}