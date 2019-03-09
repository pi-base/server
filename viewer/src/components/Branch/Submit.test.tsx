import * as React from 'react'

import Submit from './Submit'
import f from '../../../test/factories'
import { noop } from '../../../test'
import { shallow } from 'enzyme'

describe('<Branch.Submit/>', () => {
  it('can submit a branch', () => {
    const branch = f.branch()
    const submitBranch = jest.fn()

    const c = shallow(<Submit branch={branch} submitBranch={submitBranch} />)
    c.find('button').simulate('click')

    expect(submitBranch.mock.calls.length).toBe(1)
  })

  it('is disabled when submitting', () => {
    const branch = f.branch({ submitting: true })

    const c = shallow(<Submit branch={branch} submitBranch={noop} />)

    expect(c.find('button').props().disabled).toEqual(true)
  })

  it('shows if a pull request is pending', () => {
    const branch = f.branch({ pullRequestUrl: 'http://example.com' })

    const c = shallow(<Submit branch={branch} submitBranch={noop} />)

    expect(c.exists('button')).toEqual(false)

    const link = c.find('a')
    expect(link.html()).toContain('Review status')
    expect(link.props().href).toEqual('http://example.com')
  })

  it('does not show if the user does not have access', () => {
    const branch = f.branch({ access: 'read' })

    const c = shallow(<Submit branch={branch} submitBranch={noop} />)
    expect(c.html()).toEqual(null)
  })
})
