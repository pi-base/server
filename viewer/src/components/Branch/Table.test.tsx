import * as React from 'react'

import ConnectedTable, { Row, Table } from './Table'
import { makeStore, noop } from '../../../test'

import f from '../../../test/factories'
import { shallow } from 'enzyme'

describe('Branch.Table', () => {
  describe('Row', () => {
    it('can show the active branch', () => {
      const branch = f.branch({ name: 'Test', active: true })

      const c = shallow(<Row branch={branch} changeBranch={noop} submitBranch={noop} />)

      expect(c.text()).toContain('Test')
    })

    it('can switch to a non-active branch', () => {
      const branch = f.branch({ active: false })
      const changeBranch = jest.fn()

      const c = shallow(<Row branch={branch} changeBranch={changeBranch} submitBranch={noop} />)

      c.find('button').simulate('click')

      expect(changeBranch.mock.calls.length).toEqual(1)
    })
  })

  describe('Table', () => {
    it('can render multiple branches', () => {
      const branches = [
        f.branch({ name: 'b1', active: false }),
        f.branch({ name: 'b2', active: true })
      ]

      const c = shallow(<Table branches={branches} changeBranch={noop} submitBranch={noop} />)

      expect(c.html()).toContain('b1')
      expect(c.html()).toContain('b2')
    })

    it('is hidden when there are no branches', () => {
      const c = shallow(<Table branches={[]} changeBranch={noop} submitBranch={noop} />)

      expect(c.html()).toEqual(null)
    })
  })

  describe('ConnectedTable', () => {
    it('fetches the active branch from state', () => {
      const store = makeStore({
        user: {
          name: 'James',
          token: '123'
        },
        version: {
          active: 'user',
          branches: new Map([
            ['test', f.branch({ name: 'test' })],
            ['user', f.branch({ name: 'user' })],
          ])
        }
      })

      const c = shallow(
        <ConnectedTable />,
        { context: { store } }
      ).dive()

      expect(c.html()).toContain('test')
      expect(c.html()).toContain('user')
    })

    it('shows no branches if the user is not logged in', () => {
      const store = makeStore({})

      const c = shallow(
        <ConnectedTable />,
        { context: { store } }
      ).dive()

      expect(c.html()).toEqual(null)
    })
  })
})