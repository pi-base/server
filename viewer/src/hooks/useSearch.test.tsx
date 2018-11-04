import * as React from 'react'

import { mount } from 'enzyme'
import useSearch from './useSearch'

import { MemoryRouter, Router, withRouter } from 'react-router'

const Results = props => <div id="results">{props.children}</div>

const Search = withRouter(({ history, location }) => {
  const [query, setQuery] = useSearch({ history, location })

  return (
    <Results query={query}>
      <input id="input" onChange={e => setQuery({ input: e.target.value })} />
    </Results>
  )
})

describe('useSearch', () => {
  it('can read from the query string', () => {
    const c = mount(
      <MemoryRouter initialEntries={['test?input=foo']}>
        <Search />
      </MemoryRouter>
    )

    const query = c.find(Results).props().query
    expect(query).toEqual({ input: 'foo' })
  })

  it('can update the query string', () => {
    const c = mount(
      <MemoryRouter initialEntries={['test']}>
        <Search />
      </MemoryRouter>
    )

    // TODO: pass down a mock history?
    const searches: string[] = []
    c.find(Router).prop('history').listen(loc => searches.push(loc.search))

    c.find('input').simulate('change', { target: { value: 'bar' } })
    c.find('input').simulate('change', { target: { value: 'baz' } })
    c.find('input').simulate('change', { target: { value: 'baz' } })
    c.find('input').simulate('change', { target: { value: '' } })

    expect(searches).toEqual([
      '?input=bar',
      '?input=baz',
      ''
    ])
  })
})