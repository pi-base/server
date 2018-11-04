import fetch from 'isomorphic-fetch'

import Client from './Client'
// import MockClient from '../../test/Client'

import isClient from '../../test/shared_examples/client.test'

describe('Integrated client', () => {
  const build = () => new Client({ root: 'http://localhost:3141', fetch })

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

// test('Mock client', () =>
//   new MockClient()
// )