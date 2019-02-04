import * as F from '../models/Formula'

import { Branch, New, Property, Sha, Space, Theorem, Trait } from '../types'
import {
  GQL,
  assertTheorem,
  assertTrait,
  createProperty,
  createSpace,
  createUser,
  me,
  resetBranch,
  submitBranch,
  updateProperty,
  updateSpace,
  updateTheorem,
  updateTrait,
  viewer
} from '../graph'
import { GRAPH_URL, MASTER } from '../constants'

import Api from './Client/Api'
import { ApolloClient } from 'apollo-client'
import { ApolloLink } from 'apollo-link'
import { CreateUserInput } from '../types/graph/globalTypes'
import { HttpLink } from 'apollo-link-http'
import { InMemoryCache } from 'apollo-cache-inmemory'
import { onError } from 'apollo-link-error'

type Opts = Pick<ApolloOptions, Exclude<keyof ApolloOptions, 'host' | 'token'>> & {
  host?: string
}

const serializeFormula = (f: F.Formula<string>): string => JSON.stringify(F.toJSON(f))

class Client implements Api {
  _options: Opts

  _branch: Branch
  _token: string | null
  _host: string

  _apollo: ApolloClient<any>

  constructor(options: Opts = {}) {
    this._options = options
    this._branch = MASTER
    this._token = null
    this._host = options.host || GRAPH_URL

    // TODO: this assignment isn't necessary, except to placate the compiler
    this._apollo = this.buildApollo()
  }

  // N.B. this handler is only availble if the server is running in
  //   integration test mode
  async createUser(input: CreateUserInput) {
    return this.mutate(createUser, { input }).then(data => data.createUser)
  }

  loginUrl({ returnUrl }: { returnUrl: string }) {
    return `${this.host}/auth/github?returnUrl=${returnUrl}`
  }

  login(token: string) {
    this._token = token
    this.buildApollo()
  }
  logout() { this._token = null }

  get host() { return this._host }
  set host(host) {
    this._host = host
    this.buildApollo()
  }

  get branch() { return this._branch }
  set branch(branch) { this._branch = branch }

  async resetBranch(branch: Branch, to: Sha) {
    return this.mutate(resetBranch, {
      input: { branch, to }
    }).then(data => data.resetBranch.version)
  }

  // Queries

  async me() {
    return this.query(me, {}).then(data => data.me)
  }

  async viewer(version?: string) {
    return this.query(viewer, {
      version: version || this.branch
    }).then(data => data.viewer)
  }

  // Mutations

  async assertTheorem(input: New<Theorem>) {
    return this.mutate(assertTheorem, {
      patch: this.patch,
      theorem: {
        antecedent: serializeFormula(input.if),
        consequent: serializeFormula(input.then),
        description: input.description,
        references: input.references
      }
    }).then(data => {
      const theorem = data.assertTheorem.theorems[0]
      return { ...input, ...theorem }
    })
  }

  async assertTrait(input: Trait) {
    return this.mutate(assertTrait, {
      patch: this.patch,
      trait: {
        spaceId: input.space.uid,
        propertyId: input.property.uid,
        value: input.value,
        references: input.references,
        description: input.description
      }
    }).then(_ => input)
  }

  async createProperty(input: New<Property>) {
    return this.mutate(createProperty, {
      patch: this.patch,
      property: input
    }).then(data => {
      const property = data.createProperty.properties[0]
      return { ...input, ...property, }
    })
  }

  async createSpace(input: New<Space>) {
    return this.mutate(createSpace, {
      patch: this.patch,
      space: input
    }).then(data => {
      const space = data.createSpace.spaces[0]
      return { ...input, ...space }
    })
  }

  async submitBranch(name: Branch) {
    return this.mutate(submitBranch, {
      input: { branch: name }
    }).then(data => data.submitBranch)
  }

  async updateProperty(input: Property) {
    return this.mutate(updateProperty, {
      patch: this.patch,
      property: input
    }).then(data => {
      const property = data.updateProperty.properties[0]
      return { ...input, ...property }
    })
  }

  async updateSpace(input: Space) {
    return this.mutate(updateSpace, {
      patch: this.patch,
      space: input
    }).then(data => {
      const space = data.updateSpace.spaces[0]
      return { ...input, ...space }
    })
  }

  async updateTheorem(input: Theorem) {
    return this.mutate(updateTheorem, {
      patch: this.patch,
      theorem: input
    }).then(data => {
      const theorem = data.updateTheorem.theorems[0]
      return { ...input, ...theorem }
    })
  }

  async updateTrait(input: Trait) {
    return this.mutate(updateTrait, {
      patch: this.patch,
      trait: {
        spaceId: input.space.uid,
        propertyId: input.property.uid,
        references: input.references,
        description: input.description
      }
    }).then(_ => input)
  }

  // TODO: unify query and mutation
  private async query<Response, Variables={}>(
    gql: GQL<Response, Variables>,
    variables: Variables,
    context?: any
  ): Promise<Response> {
    const { query } = gql

    return this._apollo.query<Response, Variables>({
      query,
      context,
      variables
    }).then(response => response.data)
  }

  private async mutate<Response, Variables={}>(
    gql: GQL<Response, Variables>,
    variables: Variables,
    context?: any
  ): Promise<Response> {
    const { query } = gql

    return this._apollo.mutate<Response, Variables>({
      mutation: query,
      context,
      variables
    }).then(response => response.data)
  }

  private get patch() {
    return { branch: this.branch, sha: null }
  }

  private buildApollo() {
    return this._apollo = buildApolloClient({
      host: this._host,
      token: this._token,
      fetch: this._options.fetch,
      onNetworkError: this._options.onNetworkError
    })
  }
}

export default Client

type ApolloOptions = {
  host: string
  token: string | null
  fetch?: (input: RequestInfo, init?: RequestInit) => Promise<Response>
  onNetworkError?: (error: Error) => any
}

function buildApolloClient({
  host,
  token,
  fetch = window.fetch,
  onNetworkError,
}: ApolloOptions): ApolloClient<{}> {
  const authMiddleware = new ApolloLink((operation, forward) => {
    if (token) {
      operation.setContext({
        headers: {
          Authorization: `Bearer ${token}`
        }
      })
    }

    return forward!(operation)
  })

  const httpLink = new HttpLink({
    uri: `${host}/graphql`,
    credentials: 'same-origin',
    fetch,
    headers: {
      Accept: 'application/json'
    }
  })

  const links = [
    authMiddleware,
    httpLink
  ]

  if (onNetworkError) {
    const errorLink = onError(({ networkError }) => {
      if (networkError) { onNetworkError(networkError) }
    })
    links.unshift(errorLink)
  }

  return new ApolloClient({
    cache: new InMemoryCache(),
    link: ApolloLink.from(links)
  })
}