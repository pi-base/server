import * as React from 'react'
import { Trait } from '../../types'

import EditLink from '../Form/EditLink'
import Icon from '../Icon'
import Markdown from '../Markdown'
import Proof from '../Proof'
import { RouteComponentProps } from 'react-router'
import Tex from '../Tex'
import { withRouter } from 'react-router'

type OwnProps = {
  trait: Trait
}
type Props = OwnProps & RouteComponentProps<{ propertyId: string }>

type State = {
  showProperty: boolean
}

const Edit = withRouter(({ match }) => (
  <EditLink to={match.url + '/edit'} className="btn btn-default btn-xs">
    Edit
  </EditLink>
))

const Question = ({ onClick }) => (
  <button
    className="btn btn-default btn-xs"
    onClick={onClick}
  >
    <Icon type="question-sign" />
  </button>
)

class Show extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props)
    this.state = { showProperty: false }
  }

  toggleShowProperty() {
    this.setState({ showProperty: !this.state.showProperty })
  }

  render() {
    const { trait } = this.props
    if (!trait) { return null }

    const label = trait.value === false ? '¬' : ''

    // TODO: add related traits
    return (
      <div>
        <h3>
          {label}{trait.property.name}
          {' '}
          <Question onClick={() => this.toggleShowProperty()} />
          <Edit />
        </h3>

        {this.state.showProperty &&
          <Tex className="well">
            <Markdown text={trait.property.description} />
          </Tex>
        }

        <Proof trait={trait} />
      </div>
    )
  }
}

export default Show
