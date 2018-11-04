import * as React from 'react'
import * as F from '../../models/Formula'
import * as S from '../../selectors'

import { MemoryRouter } from 'react-router'
import { Provider } from 'react-redux'
import Results from './Results'
import { fixtureStore } from '../../../test'
import { mount } from 'enzyme'
import { Property } from '../../types'

const store = fixtureStore()

const properties = S.propertyFinder(store.getState())

const compact = properties.find('compact')!
const metacompact = properties.find('metacompact')!

describe('<Results>', () => {
  it('can render results', () => {
    const f = F.atom(compact, true)

    const c = mount(
      <Provider store={store}>
        <MemoryRouter>
          <Results formula={f} />
        </MemoryRouter>
      </Provider>
    )

    expect(c.text()).toContain('Unit interval')
  })

  it('can explain a tautology', () => {
    const f = F.and(
      F.atom(compact, true),
      F.atom(compact, false),
    )

    const c = mount(
      <Provider store={store}>
        <MemoryRouter>
          <Results formula={f} />
        </MemoryRouter>
      </Provider>
    )

    expect(c.text()).toContain('No spaces exist satisfying (Compact ∧ ¬Compact), tautologically')
  })

  it('can show when there are no known results', () => {
    const property: Property = {
      uid: 'P9999999',
      name: 'New',
      references: [],
      description: ''
    }

    const f = F.atom(property, false)

    const c = mount(
      <Provider store={store}>
        <MemoryRouter>
          <Results formula={f} />
        </MemoryRouter>
      </Provider>
    )

    expect(c.text()).toContain('No spaces found satisfying ¬New')
  })

  it('can show a non-trivial disproof', () => {
    const f = F.and(
      F.atom(compact, true),
      F.atom(metacompact, false)
    )

    const c = mount(
      <Provider store={store}>
        <MemoryRouter>
          <Results formula={f} />
        </MemoryRouter>
      </Provider>
    )

    expect(c.text()).toContain('No spaces exist satisfying (Compact ∧ ¬Metacompact) due to the following theorems')
  })
})