import { BranchState, Property } from '../src/types'

let sequence = 1

type Factory<Value, Required = {}> = (opts: Required & Partial<Value>) => Value

const branch: Factory<BranchState> = (opts = {}) => ({
  name: 'test',
  sha: 'abc' + sequence++,
  access: 'admin',
  active: true,
  submitting: false,
  pullRequestUrl: undefined,
  ...opts
})

const property: Factory<Property, { name: string }> = opts => ({
  uid: 'p' + sequence++,
  aliases: [],
  references: [],
  description: '',
  ...opts
})

const factories = {
  branch,
  property
}

export default factories