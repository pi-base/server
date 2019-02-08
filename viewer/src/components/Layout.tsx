import * as React from 'react'

import { RouteComponentProps, withRouter } from 'react-router'

import { Dispatch } from '../types'
import EditBanner from './EditBanner'
import Footer from './Footer'
import Navbar from './Navbar'
import { State } from '../reducers'
import { boot } from '../actions'
import { connect } from 'react-redux'
import Tex from './Tex'
import { Link } from 'react-router-dom'

// tslint:disable-next-line no-any
declare var MathJax: any

type StateProps = {
  booted: boolean
  debug: boolean
}
type DispatchProps = {
  boot: () => void
}
type Props = StateProps & DispatchProps & RouteComponentProps<{}>

class Layout extends React.PureComponent<Props> {
  unlisten: any

  componentWillMount() {
    const { boot, booted, history } = this.props

    if (!booted) { boot() }

    this.unlisten = history.listen(() => {
      MathJax.Hub.Typeset()
    })
  }

  componentWillUnmount() {
    this.unlisten && this.unlisten()
  }

  render() {
    const { booted, debug, location } = this.props

    return (
      <div>
        <Navbar />


        { location.pathname === '/' ? 
          <Tex className="jumbotron wide">
            <div className="container">
              <h1>π-Base</h1>
              <p>a community database of topological examples with expressive searches like</p>
              <p>⮕ <Link to="/spaces?formula=compact%20%2B%20connected%20%2B%20t_2%20%2B%20~metrizable">Non-metric continua</Link></p>
              <p>⮕ <Link to="/spaces?text=compactification">Compactifications</Link></p>
              <p>⮕ <Link to="/theorems/I000112">Evidence that T₅ ⇒ T₄ does not reverse</Link></p>
            </div>
          </Tex>
	  : ''}

        <EditBanner />

        <div className="container">
          {booted || location.pathname === '/'
            ? this.props.children
            : 'Loading...'}
        </div>

        {debug ? <Footer /> : ''}
      </div>
    )
  }
}

const mapStateToProps = (state: State): StateProps => ({
  booted: state.spaces.size > 0,
  debug: state.debug
})

const mapDispatchToProps = (dispatch: Dispatch): DispatchProps => ({
  boot: () => dispatch(boot())
})

// `withRouter` is required so that location changes will trigger a re-render
// see https://github.com/ReactTraining/react-router/blob/master/packages/react-router/docs/guides/blocked-updates.md
export default withRouter(connect(
  mapStateToProps,
  mapDispatchToProps
)(Layout))
