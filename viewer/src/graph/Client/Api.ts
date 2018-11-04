import { BranchName, New, Property, Space, Sha, Theorem, Trait } from '../../types'

import { CreateUserInput } from '../../types/graph/globalTypes'
import { CreateUser } from '../../types/graph/CreateUser'
import { Me } from '../../types/graph/Me'
import { Viewer } from '../../types/graph/Viewer'
import { SubmitBranch } from '../../types/graph/SubmitBranch'

type Response<T> = Promise<T>

export default interface Api {
  // Client state management
  loginUrl: (opts: { redirectTo: string }) => string
  login: (token: string) => void
  logout: () => void
  branch: BranchName
  host: string

  // Testing utilities
  createUser: (user: CreateUserInput) => Response<CreateUser['createUser']>
  resetBranch: (branch: BranchName, to: Sha) => Response<Sha>

  // Queries
  me: () => Response<Me['me']>
  viewer: (version?: string) => Response<Viewer['viewer']>

  // Mutations
  assertTheorem: (theorem: New<Theorem>) => Response<Theorem>
  assertTrait: (trait: Trait) => Response<Trait>
  createSpace: (space: New<Space>) => Response<Space>
  createProperty: (property: New<Property>) => Response<Property>
  submitBranch: (name: BranchName) => Response<SubmitBranch['submitBranch']>
  updateProperty: (property: Property) => Response<Property>
  updateSpace: (space: Space) => Response<Space>
  updateTheorem: (theorem: Theorem) => Response<Theorem>
  updateTrait: (theorem: Trait) => Response<Trait>
}