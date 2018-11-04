import { Dispatch, Space } from '../../types'

import Form from './Form'
import { RouteComponentProps } from 'react-router'
import { connect } from 'react-redux'
import { updateSpace } from '../../actions'

interface OwnProps {
  space: Space
}
interface DispatchProps {
  onSubmit: (space: Space) => Promise<any>
}

const mapDispatchToProps = (
  dispatch: Dispatch,
  { history, space }: OwnProps & RouteComponentProps<{}>
): DispatchProps => ({
  onSubmit: values => dispatch(updateSpace(values)).then(_ => {
    history.push(`/spaces/${space.uid}`)
  })
})

export default connect(
  null,
  mapDispatchToProps
)(Form)