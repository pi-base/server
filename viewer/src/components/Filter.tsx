import * as React from 'react'

import { Finder, Record, Weights } from '../models/Finder'

export interface Props<R> {
  collection: R[]
  weights?: Weights
  onChange: (results: R[]) => void
  name?: string
  placeholder?: string
}

export interface State {
  q: string
}

class Filter<R extends Record> extends React.Component<Props<R>, State> {
  finder: Finder<R>

  constructor(props: Props<R>) {
    super(props)
    this.state = { q: '' }
    this.finder = this.buildFinder(props)
  }

  componentWillMount() {
    this.finder = this.buildFinder(this.props)
  }

  buildFinder(props) {
    const weights = this.props.weights || [
      { name: 'name', weight: 0.7 },
      { name: 'aliases', weight: 0.6 },
      { name: 'description', weight: 0.3 }
    ]
    return new Finder(this.props.collection, weights)
  }

  onChange(q: string) {
    this.setState({ q })
    const filtered = q ? this.finder.search(q) : this.finder.all()
    this.props.onChange(filtered)
  }

  render() {
    const { name = 'filter', placeholder = 'Filter' } = this.props

    return (
      <input
        type="text"
        autoComplete="off"
        className="form-control"
        name={name}
        placeholder={placeholder}
        value={this.state.q}
        onChange={e => this.onChange(e.target.value)}
      />
    )
  }
}

export default Filter
