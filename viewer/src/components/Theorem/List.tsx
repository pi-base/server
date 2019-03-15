import * as React from 'react'

import EditLink from '../Form/EditLink'
import Filter from '../Filter'
import List from '../List'
import Implication from '../Implication'
import { Link } from 'react-router-dom'
import Preview from '../Preview'
import { State as StoreState } from '../../reducers'
import Tex from '../Tex'
import { Theorem } from '../../types'
import Title from '../Title'
import { by } from '../../utils'
import { connect } from 'react-redux'

interface Props {
  object: Theorem
}

class Item extends React.Component<Props, { expanded: boolean }> {
  constructor(props: Props) {
    super(props)
    this.state = { expanded: false }
  }

  toggle() {
    this.setState({ expanded: !this.state.expanded })
  }

  render() {
    const theorem = this.props.object

    return (
      <div className="row">
        <div className="col-md-12">
          <Tex key={theorem.uid}>
            <h3>
              <Link to={`/theorems/${theorem.uid}`}>
                <Implication theorem={theorem} link={false} />
              </Link>
            </h3>
            <Preview text={theorem.description} />
          </Tex>
        </div>
      </div>
    )
  }
}

interface StateProps {
  theorems: Theorem[]
}
const Index = ({ theorems }: StateProps) => {
  return (
    <div>
      <Title title="Properties" />

      <EditLink to="/properties/new" className="btn btn-default">New</EditLink>

      <List<Theorem>
        name="properties"
        objects={theorems}
        component={Item}
      />
    </div>
  )
}

export default connect(
  (state: StoreState): StateProps => ({
    theorems: Array.from(state.theorems.values()).sort(by('uid')).reverse()
  })
)(Index)
