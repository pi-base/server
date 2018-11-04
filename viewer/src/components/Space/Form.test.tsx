import * as React from 'react'

import Form from './Form'
import { MemoryRouter } from 'react-router'
import { Provider } from 'react-redux'
import f from '../../../test/factories'
import { makeStore } from '../../../test'
import { mount } from 'enzyme'

describe('<Space.Form/>', () => {
  // Editable branch
  const store = makeStore({
    version: {
      active: 'user',
      branches: new Map([
        ['user', f.branch({ name: 'user' })]
      ])
    }
  })

  it.skip('previews a space', (done) => {
    const onSubmit = jest.fn()

    const c = mount(
      <Provider store={store}>
        <MemoryRouter>
          <Form onSubmit={onSubmit} />
        </MemoryRouter>
      </Provider>
    )

    c.find('input[name="name"]').simulate('change', {
      target: {
        name: 'name',
        value: 'Space'
      }
    })
    c.find('textarea[name="description"]').simulate('change', {
      target: {
        name: 'description',
        value: 'Description'
      }
    })
    c.find('button[type="submit"]').simulate('click')
  })
})
