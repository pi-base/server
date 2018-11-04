import Api from '../../src/graph/Client/Api'
import { MASTER } from '../../src/constants'

const examples = (
  builder: () => Api
) => {
  let userId = 0
  let client: Api

  beforeEach(() => { client = builder() })

  const login = async (opts: { reviewer?: boolean } = {}) => {
    userId += 1

    const { reviewer = false } = opts

    const user = {
      name: `User ${userId}`,
      reviewer
    }

    return client.createUser(user).then(({ token }) => {
      client.login(token)
      return { ...user, token }
    })
  }

  describe('queries', () => {
    describe('me', async () => {
      it('shows user data when authenticated', async () => {
        const user = await login()
        return client.me().then(me => {
          expect(me.__typename).toEqual('User')
          expect(me.name).toEqual(user.name)

          const master = me.branches.find(b => b.name === MASTER)
          expect(master!.access).toEqual('read')
        })
      })

      it('errors when unauthenticated', async () =>
        client.me().catch(err => {
          expect(err.networkError.response.status).toEqual(401)
        })
      )
    })

    describe('viewer', () => {
      it('can fetch the viewer', async () => {
        return client.viewer().then(viewer => {
          expect(viewer.__typename).toEqual('Viewer')
          expect(viewer.spaces.length).toBeGreaterThan(100)
          expect(viewer.properties.length).toBeGreaterThan(100)
          expect(viewer.theorems.length).toBeGreaterThan(150)
        })
      })

    })
  })

  describe('mutations', () => {
    let user
    beforeEach(async () => {
      user = await login()

      const me = await client.me()
      const branch = me.branches.find(b => b.access === 'admin')
      if (!branch) { throw `Could not find adminable branch for user ${user.name}` }

      client.branch = branch.name
    })

    describe('createSpace', () => {
      it('can save', async () => {

        const space = await client.createSpace({
          name: 'New Space',
          description: '',
          references: []
        })

        expect(space.name).toEqual('New Space')
        expect(space.uid.length).toEqual(37)
      })
    })
  })
}

export default examples