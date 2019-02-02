import { MASTER } from '../src/constants'
import * as F from '../src/models/Formula'
import {
  Api,
  Branch,
  CreateUserInput,
  Citation,
  Id,
  New,
  Property,
  Space,
  Theorem,
  Trait
} from '../src/types'

const serializeFormula = (f: F.Formula<string>): string => JSON.stringify(F.toJSON(f))

class Client implements Api {
  _token: string | null
  _branch: Branch
  _properties: Map<Id, Property>
  _spaces: Map<Id, Space>
  _theorems: Map<Id, Theorem>
  _traits: Map<[Id, Id], Trait>
  _id: number

  host: string

  constructor() {
    this._branch = MASTER
    this._token = null
    this._id = 0

    this._properties = new Map()
    this._spaces = new Map()
    this._theorems = new Map()
    this._traits = new Map()

    this.host = 'http://example.com'
  }

  login(token: string) { this._token = token }
  logout() { this._token = null }
  loginUrl() { return `${this.host}/login` }

  get branch() { return this._branch }

  async createUser(_input: CreateUserInput) {
    return {
      __typename: ('CreateUserResponse' as 'CreateUserResponse'),
      token: 'asdf'
    }
  }

  async resetBranch() {
    return 'sha'
  }

  async me() {
    return {
      __typename: ('User' as 'User'),
      name: 'user',
      branches: [
        { __typename: ('Branch' as 'Branch'), name: MASTER, sha: '123', access: 'read' }
      ]
    }
  }

  async viewer(version?: string) {
    return {
      __typename: ('Viewer' as 'Viewer'),
      version: version || '123',
      spaces: this.present(this._spaces, this.space),
      properties: this.present(this._properties, this.property),
      theorems: this.present(this._theorems, this.theorem)
    }
  }

  private present<T, V>(collection: Map<Id, T>, presenter: (object: T) => V) {
    return Array.from(collection.values()).map(presenter)
  }

  private reference(citation: Citation) {
    return {
      __typename: ('Citation' as 'Citation'),
      ...citation
    }
  }

  private space(space: Space) {
    return {
      ...space,
      __typename: ('Space' as 'Space'),
      aliases: space.aliases || [],
      references: space.references.map(ref => this.reference(ref)),
      traits: []
    }
  }

  private property(property: Property) {
    return {
      ...property,
      __typename: ('Property' as 'Property'),
      aliases: property.aliases || [],
      references: property.references.map(ref => this.reference(ref))
    }
  }

  private theorem(theorem: Theorem) {
    return {
      ...theorem,
      __typename: ('Theorem' as 'Theorem'),
      if: serializeFormula(theorem.if),
      then: serializeFormula(theorem.then),
      references: theorem.references.map(ref => this.reference(ref))
    }
  }

  async assertTheorem(input: New<Theorem>) {
    const theorem = { uid: this.id(), ...input }
    this._theorems.set(theorem.uid, theorem)
    return theorem
  }

  async assertTrait(input: Trait) {
    return this.updateTrait(input)
  }

  async createProperty(input: New<Property>) {
    const property = { uid: this.id(), ...input }
    this._properties.set(property.uid, property)
    return property
  }

  async createSpace(input: New<Space>) {
    const space = { uid: this.id(), ...input }
    this._spaces.set(space.uid, space)
    return space
  }

  async submitBranch() {
    return {
      __typename: ('SubmitBranchResponse' as 'SubmitBranchResponse'),
      branch: this.branch,
      url: 'http://example.com/branch'
    }
  }

  async updateProperty(input: Property) {
    this._properties.set(input.uid, input)
    return input
  }

  async updateSpace(input: Space) {
    this._spaces.set(input.uid, input)
    return input
  }

  async updateTheorem(input: Theorem) {
    this._theorems.set(input.uid, input)
    return input
  }

  async updateTrait(input: Trait) {
    this._traits.set(
      [input.space.uid, input.property.uid],
      input
    )
    return input
  }

  private id() {
    this._id += 1
    return String(this._id)
  }
}

export default Client