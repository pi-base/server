import * as React from 'react'

import { Dispatch, Space } from '../../types'

import Form from './Form'
import { RouteComponentProps } from 'react-router'
import { connect } from 'react-redux'
import { createSpace } from '../../actions'

interface DispatchProps {
  onSubmit: (space: Space) => void
}

const mapDispatchToProps = (
  dispatch: Dispatch,
  { history }: RouteComponentProps<{}>
): DispatchProps => ({
  onSubmit: (values: Space) => dispatch(createSpace(values)).
    then(space => history.push(`/spaces/${space.uid}`))
})

export default connect(
  null,
  mapDispatchToProps
)(Form)