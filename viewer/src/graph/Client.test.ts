import fetch from 'isomorphic-fetch'

import Client from './Client'
import Server from '../../test/Server'

import isClient from '../../test/shared_examples/client.test'

const server = new Server()

if (process.env.CI) { jest.setTimeout(30000) }

describe('Integrated client', () => {
  beforeAll(done => server.boot().then(() => done()))
  afterAll(() => server.shutdown())

  const build = () => new Client({ host: server.host, fetch })

  isClient(build)

  it('can fetch the viewer at a version', async () => {
    const version = '4fbad4eba4b72b429eaf176df19103a06bd9ca80'

    return build().viewer(version).then(viewer => {
      expect(viewer.__typename).toEqual('Viewer')
      expect(viewer.version).toEqual(version)
      expect(viewer.spaces.length).toEqual(135)
      expect(viewer.properties.length).toEqual(126)
      expect(viewer.theorems.length).toEqual(186)
    })
  })
})

// import MockClient from '../../test/Client'
// describe('Mock client', () =>
//   isClient(() => new MockClient())
// )
