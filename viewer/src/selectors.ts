import * as F from './models/Formula'

import { Formula, Id, Proof, Property, SearchModifier, Space, Table, Theorem, Trait } from './types'

import { Finder } from './models/Finder'
import { Prover } from './models/Prover'
import { State } from './reducers'
import { createSelector } from 'reselect'
import memoizeOne from 'memoize-one'
import { union } from './utils'

export const propertyFinder = createSelector(
  (state: State) => state.properties.values(),
  properties => new Finder(Array.from(properties))
)

export const spaceFinder = createSelector(
  (state: State) => state.spaces.values(),
  spaces => new Finder(Array.from(spaces))
)

export const traitValues = createSelector(
  (state: State) => state.traits,
  traits => {
    const result = new Map<Id, Map<Id, boolean>>()
    traits.forEach((space, sid) => {
      const map = new Map()
      space.forEach((trait, pid) => {
        map.set(pid, trait.value)
      })
      result.set(sid, map)
    })
    return result
  }
)

export const prover = createSelector(
  traitValues,
  (state: State) => state.theorems,
  (traits, theorems) => new Prover(traits, Array.from(theorems.values()))
)

export const asserted = (state: State, sid: Id, pid: Id): boolean => {
  const p = state.proofs.get(traitId({ space: sid, property: pid }))
  return p ? p.type === 'asserted' : false
}

export const traitId = ({ space, property }: { space: Id, property: Id }): Id =>
  `${space}|${property}`

export const spaceTraits = (state: State, space: Space): Trait[] => {
  const values = state.traits.get(space.uid)
  if (!values) { return [] }

  const result: Trait[] = []
  values.forEach((value, pid) => {
    const uid = value.uid || traitId({ space: space.uid, property: pid })
    result.push({
      ...value,
      uid,
      space: space,
      property: state.properties.get(pid)!,
      description: value.description || '',
      references: value.references || []
    })
  })
  return result
}

export const unknownProperties = (state: State, space: Space): Property[] => {
  const values = state.traits.get(space.uid) || new Map()

  return Array.from(state.properties.values()).reduce<Property[]>(
    (acc, prop) => {
      if (!values.has(prop.uid)) { acc.push(prop) }
      return acc
    },
    []
  )
}

export const getTrait = (state: State, space: Space, propertyId: Id): Trait | undefined => {
  const ts = state.traits.get(space.uid)
  if (!ts) { return undefined }
  const property = state.properties.get(propertyId)
  if (!property) { return undefined }
  const value = ts.get(propertyId)
  if (value === undefined) { return undefined }
  const uid = value.uid || traitId({ space: space.uid, property: propertyId })
  return {
    ...value,
    uid,
    space,
    property,
    description: value.description || '',
    references: value.references || []
  }
}

type SearchOptions = {
  formula?: Formula<Id>
  modifier?: SearchModifier
  text?: string
}

const formulaScan = memoizeOne(
  (
    spaces: Space[],
    traits: State['traits'],
    formula: Formula<string>,
    matcher: (v: boolean | undefined) => boolean
  ) => {
    return spaces.filter(s => {
      const ts = traits.get(s.uid)
      if (!ts) { return false }
      const map = new Map()
      ts.forEach((t, id) => map.set(id, t.value))
      return matcher(F.evaluate(formula, map))
    })
  }
)

const _search = (
  state: State,
  options: SearchOptions
): Space[] => {
  let spaces: Space[]

  // FIXME: make sure these actually memoize
  if (options.text) {
    spaces = spaceFinder(state).search(options.text)
  } else {
    spaces = Array.from(state.spaces.values())
  }

  if (options.formula) {
    const matcher = {
      'true': (value) => value === true,
      'false': (value) => value === false,
      'unknown': (value) => value === undefined,
      'not_false': (value) => value !== false
    }[options.modifier || 'true']

    return formulaScan(spaces, state.traits, options.formula, matcher)
  }

  return spaces
}

export const search = memoizeOne(_search)

export const searchFormula = createSelector(
  (state: State) => state.search.formulaMemo,
  (state: State) => state.properties,
  (formula, properties) => {
    if (!formula) { return }
    return F.compact(
      F.mapProperty(
        (p) => properties.get(p),
        formula
      )
    )
  }
)

export const parseFormula = (
  state: State,
  text: string
): F.Formula<Id> | undefined => {
  const parsed = F.parse(text)
  if (!parsed) { return }

  const finder = propertyFinder(state)
  let errors = false
  const result = F.mapProperty(
    id => {
      const property = finder.find(id)
      if (!property) { errors = true }
      return property as Property
    },
    parsed
  )
  return errors ? undefined : F.mapProperty(p => p.uid, result)
}

export const counterexamples = (state: State, theorem: Theorem): Space[] => {
  const formula = F.and(
    theorem.if,
    F.negate(theorem.then)
  )
  return search(state, { formula })
}

export const theoremProperties = (state: State, theorem: Theorem): Property[] => {
  const ids = union(F.properties(theorem.if), F.properties(theorem.then))
  const props: Property[] = []
  ids.forEach(uid => {
    const prop = state.properties.get(uid!)
    if (prop) { props.push(prop) } // FIXME: else?
  })
  return props
}

export const activeBranch = (state: State) => {
  if (!state.version.active) { return undefined } // FIXME
  return state.version.branches.get(state.version.active)!
}
export const editing = (state: State) => {
  const branch = activeBranch(state)
  if (!branch) { return false }
  return branch.access === 'admin'
}

export const proof = (state: State, spaceId: string, propertyId: string): Proof | undefined => {
  const space = state.spaces.get(spaceId)
  if (!space) { return }

  const root = state.proofs.get(`${spaceId}|${propertyId}`)
  if (!root || root.type === 'asserted') { return }

  const theoremIds: Id[] = [root.theorem]

  // TODO: traversal ordering? preserve insertion order of properties
  const queue = root.properties
  const visited: Set<Id> = new Set()
  const assumed: Set<Id> = new Set()
  while (queue.length > 0) {
    const pid = queue.shift()!
    const step = state.proofs.get(`${spaceId}|${pid}`)
    if (!step) { continue }
    if (step.type === 'asserted') {
      assumed.add(pid)
    } else {
      theoremIds.unshift(step.theorem)
      step.properties.forEach(p => {
        if (!visited.has(p)) {
          queue.push(p)
        }
      })
    }
    visited.add(pid)
  }

  const traits = Array.from(assumed).map(pid => getTrait(state, space, pid))
  const theorems = theoremIds.map(t => state.theorems.get(t))

  if (traits.indexOf(undefined) !== -1 || theorems.indexOf(undefined) !== -1) {
    // TODO
    return undefined
  }

  return {
    theorems: theorems as Theorem[],
    traits: traits as Trait[]
  }
}