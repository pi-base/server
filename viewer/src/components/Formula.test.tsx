import * as F from '../models/Formula'
import * as React from 'react'

import { BrowserRouter } from 'react-router-dom'
import Formula from './Formula'
import f from '../../test/factories'
import { render } from 'enzyme'

describe('<Formula/>', () => {
  const p = f.property({ name: 'P', uid: 'p' })
  const q = f.property({ name: 'Q', uid: 'q' })

  it('can render a true atom', () => {
    const formula = F.atom(p, true)
    const c = render(<Formula formula={formula} />)

    expect(c.text()).toEqual('P')
  })

  it('can render a false atom', () => {
    const formula = F.atom(q, false)
    const c = render(<Formula formula={formula} />)

    expect(c.text()).toEqual('¬Q')
  })

  it('can render ands', () => {
    const formula = F.and(
      F.atom(p, true),
      F.atom(q, true)
    )
    const c = render(<Formula formula={formula} />)

    expect(c.text()).toEqual('(P ∧ Q)')
  })

  it('can render ors', () => {
    const formula = F.or(
      F.atom(p, true),
      F.atom(q, true)
    )
    const c = render(<Formula formula={formula} />)

    expect(c.text()).toEqual('(P ∨ Q)')
  })

  it('can add links', () => {
    const formula = F.and(
      F.atom(p, true),
      F.atom(q, true)
    )
    const c = render(
      <BrowserRouter>
        <Formula formula={formula} link={true} />
      </BrowserRouter>
    )

    expect(c.text()).toEqual('(P ∧ Q)')
  })
})
