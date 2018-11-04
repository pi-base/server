import { Dispatch, Property } from '../../types'

import Form from './Form'
import { RouteComponentProps } from 'react-router'
import { connect } from 'react-redux'
import { updateProperty } from '../../actions'

interface OwnProps {
  property: Property
}
interface DispatchProps {
  onSubmit: (property: Property) => Promise<any>
}

const mapDispatchToProps = (
  dispatch: Dispatch,
  { history, property }: OwnProps & RouteComponentProps<{}>
): DispatchProps => ({
  onSubmit: values => dispatch(updateProperty(values)).
    then(_ => history.push(`/properties/${property.uid}`))
})

export default connect(
  null,
  mapDispatchToProps
)(Form)