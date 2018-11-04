import * as React from 'react'
import * as F from '../models/Formula'

import Search, { Results, parseFormulaQuery } from './Search'
import { mount } from 'enzyme'
import { MemoryRouter, Router } from 'react-router'
import { Provider } from 'react-redux'
import { fixtureStore } from '../../test'
import { propertyFinder } from '../selectors'
import { SearchModifier } from '../types'

const store = fixtureStore()

describe('<Search>', () => {
  it('can filter by text', () => {
    const c = mount(
      <Provider store={store}>
        <MemoryRouter>
          <Search />
        </MemoryRouter>
      </Provider>
    )

    c.find('input[name="text"]').simulate('change', { target: { value: 'plan' } })

    const text = c.find('Results').text()

    expect(text).toContain('Thomas plank')
    expect(text).not.toContain('Discrete')
  })

  it('can filter by formula', () => {
    const c = mount(
      <Provider store={store}>
        <MemoryRouter>
          <Search />
        </MemoryRouter>
      </Provider>
    )

    c.find('input[name="formula"]').simulate('change', { target: { value: 'compac' } })

    const text = c.find('Results').text()

    expect(text).not.toContain('Thomas plank')
    expect(text).toContain('Discrete')
  })

  it('can select an example', () => {
    const c = mount(
      <Provider store={store}>
        <MemoryRouter>
          <Search />
        </MemoryRouter>
      </Provider>
    )

    const searches: string[] = []
    c.find(Router).prop('history').listen(loc => searches.push(loc.search))

    c.find('Example').first().find('a').simulate('click')

    expect(searches.length).toEqual(1)
    expect(searches[0]).toContain('~metrizable')

    expect(c.find(Results).prop('formula').kind).toEqual('and')
  })
})

describe('parseFormulaQuery', () => {
  const properties = propertyFinder(store.getState())

  const parse = (formula: string): { formula?: F.Formula<string>, modifier?: SearchModifier } => {
    const parsed = parseFormulaQuery({ properties, formula })
    if (!parsed) { return { formula: undefined, modifier: undefined } }

    const f = F.mapProperty(p => p.uid, parsed.formula)
    return { formula: f, modifier: parsed.modifier }
  }

  const expected = F.and(
    F.atom('P000016', true),
    F.atom('P000036', true)
  )

  it('can parse not_false', () => {
    const { formula, modifier } = parse('!!compact + connected')!
    expect(modifier).toEqual('not_false')
    expect(formula).toEqual(expected)
  })

  it('can parse false', () => {
    const { formula, modifier } = parse('!compact + connected')!
    expect(modifier).toEqual('false')
    expect(formula).toEqual(expected)
  })

  it('can parse unknown', () => {
    const { formula, modifier } = parse('?compact + connected')!
    expect(modifier).toEqual('unknown')
    expect(formula).toEqual(expected)
  })

  it('can parse true', () => {
    const { formula, modifier } = parse('compact + connected')!
    expect(modifier).toEqual('true')
    expect(formula).toEqual(expected)
  })
})
