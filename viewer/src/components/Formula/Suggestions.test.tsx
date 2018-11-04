import * as React from 'react'

import Suggestions, { Suggestion } from './Suggestions'

import f from '../../../test/factories'
import { mount } from 'enzyme'
import { noop } from '../../../test'

describe('<Formula.Suggestions', () => {
  const props = [
    f.property({ name: 'P' }),
    f.property({ name: 'Q' }),
    f.property({ name: 'R' })
  ]

  it('highlights the active suggestion', () => {
    const c = mount(
      <Suggestions suggestions={props} limit={10} selected={1} onSelect={noop} />
    )

    expect(c.find(Suggestion).map(w => w.props().selected)).toEqual([false, true, false])
  })

  it('can select a suggestion', () => {
    const f = jest.fn()

    const c = mount(
      <Suggestions suggestions={props} limit={10} selected={2} onSelect={f} />
    )

    c.find('.active').simulate('click')

    expect(f.mock.calls.length).toEqual(1)
    expect(f.mock.calls[0][0].name).toEqual('R')
  })
})
