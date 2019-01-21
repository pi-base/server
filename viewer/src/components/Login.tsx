import * as React from 'react'

import { Dispatch, Token } from '../types'

import { RouteComponentProps } from 'react-router'
import { connect } from 'react-redux'
import { login } from '../actions'

type RouteProps = RouteComponentProps<{ token: string }>
type DispatchProps = {
  login: (token: Token) => Promise<any>
}
type Props = RouteProps & DispatchProps

class Login extends React.PureComponent<Props> {
  componentWillMount() {
    this.props.login(this.props.match.params.token)
  }

  render() {
    return (<p>Logging in ...</p>)
  }
}

export default connect(
  () => ({}),
  (dispatch: Dispatch): DispatchProps => ({
    login: (token: Token) => dispatch(login(token))
  })
)(Login)