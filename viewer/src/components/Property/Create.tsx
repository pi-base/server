import { Dispatch } from '../../types'
import Form from './Form'
import { RouteComponentProps } from 'react-router';
import { connect } from 'react-redux'
import { createProperty } from '../../actions'

type Props = RouteComponentProps<{}>

export default connect(
  null,
  (dispatch: Dispatch, ownProps: Props) => ({
    onSubmit: values => dispatch(createProperty(values)).
      then(property => ownProps.history.push(`/properties/${property.uid}`))
  })
)(Form)
