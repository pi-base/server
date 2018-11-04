import { Me } from '../types/graph/Me'
import { Viewer, ViewerVariables } from '../types/graph/Viewer'

import { AssertTrait, AssertTraitVariables } from '../types/graph/AssertTrait'
import { AssertTheorem, AssertTheoremVariables } from '../types/graph/AssertTheorem'
import { CreateProperty, CreatePropertyVariables } from '../types/graph/CreateProperty'
import { CreateSpace, CreateSpaceVariables } from '../types/graph/CreateSpace'
import { CreateUser, CreateUserVariables } from '../types/graph/CreateUser'
import { ResetBranch, ResetBranchVariables } from '../types/graph/ResetBranch'
import { SubmitBranch, SubmitBranchVariables } from '../types/graph/SubmitBranch'
import { UpdateProperty, UpdatePropertyVariables } from '../types/graph/UpdateProperty'
import { UpdateSpace, UpdateSpaceVariables } from '../types/graph/UpdateSpace'
import { UpdateTheorem, UpdateTheoremVariables } from '../types/graph/UpdateTheorem'
import { UpdateTrait, UpdateTraitVariables } from '../types/graph/UpdateTrait'

export * from '../types/graph/globalTypes'

// We want to package up the query with its associated types as we load it
// Unfortunately, we can't load from a "dynamic" path, so we have this bit of indirection
// See https://github.com/webpack/webpack/issues/196#issuecomment-417840264 for details

export type GQL<Response, Variables = {}> = {
  __type__: never
  query: any
}

function wrap<Response, Variables = {}>(query): GQL<Response, Variables> {
  return { query } as GQL<Response, Variables>
}

export const me = wrap<Me>(require('./queries/Me.gql'))
export const viewer = wrap<Viewer, ViewerVariables>(require('./queries/Viewer.gql'))

export const assertTrait = wrap<AssertTrait, AssertTraitVariables>(require('./mutations/AssertTrait.gql'))
export const assertTheorem = wrap<AssertTheorem, AssertTheoremVariables>(require('./mutations/AssertTheorem.gql'))
export const createProperty = wrap<CreateProperty, CreatePropertyVariables>(require('./mutations/CreateProperty.gql'))
export const createSpace = wrap<CreateSpace, CreateSpaceVariables>(require('./mutations/CreateSpace.gql'))
export const resetBranch = wrap<ResetBranch, ResetBranchVariables>(require('./mutations/ResetBranch.gql'))
export const submitBranch = wrap<SubmitBranch, SubmitBranchVariables>(require('./mutations/SubmitBranch.gql'))
export const updateProperty = wrap<UpdateProperty, UpdatePropertyVariables>(require('./mutations/UpdateProperty.gql'))
export const updateSpace = wrap<UpdateSpace, UpdateSpaceVariables>(require('./mutations/UpdateSpace.gql'))
export const updateTheorem = wrap<UpdateTheorem, UpdateTheoremVariables>(require('./mutations/UpdateTheorem.gql'))
export const updateTrait = wrap<UpdateTrait, UpdateTraitVariables>(require('./mutations/UpdateTrait.gql'))

export const createUser = wrap<CreateUser, CreateUserVariables>(require('./mutations/CreateUser.gql'))
