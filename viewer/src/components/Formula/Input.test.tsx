import * as F from '../../models/Formula'
import * as React from 'react'

import { DOWN, Input, TAB } from './Input'

import { Finder } from '../../models/Finder'
import f from '../../../test/factories'
import { mount } from 'enzyme'

const foo = f.property({ name: 'Foo' })
const bar = f.property({ name: 'Bar' })
const baz = f.property({ name: 'Baz' })

const properties = new Finder([foo, bar, baz])

describe('<Formula.Input/>', () => {
  it.skip('can input a conjunction', () => {
    const onChange = jest.fn()

    const c = mount(
      <Input
        name="formula"
        onChange={onChange}
        properties={properties}
      />
    )

    c.find('input').simulate('change', { target: { value: '~ba' } })
    // TODO: assertions about visible dropdown state
    c.find('input').simulate('keyDown', { which: DOWN })
    c.find('input').simulate('keyDown', { which: DOWN })
    c.find('input').simulate('keyDown', { which: TAB })

    expect(c.find('input').props().value).toEqual('~Bar')

    c.find('input').simulate('change', { target: { value: '~Bar + F' } })
    c.find('input').simulate('keyDown', { which: DOWN })
    c.find('input').simulate('keyDown', { which: TAB })

    expect(c.find('input').props().value).toEqual('~Bar + Foo')

    expect(onChange.mock.calls[0][0]).toEqual(
      F.atom(bar, false)
    )
    expect(onChange.mock.calls[1][0]).toEqual(
      F.and(
        F.atom(bar, false),
        F.atom(foo, true)
      )
    )
  })
})

// TODO: test reducers for e.g. prefix state
