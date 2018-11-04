import * as React from 'react'

import Filter from './Filter'

interface Record { uid: string }

export interface Props<R> {
  name: string
  objects: R[]
  component: React.ComponentClass<{ object: R }>
}

export interface State<R> {
  limit: number
  objects: R[]
}

class List<R extends Record> extends React.Component<Props<R>, State<R>> {
  constructor(props: Props<R>) {
    super(props)
    this.state = {
      limit: 25,
      objects: []
    }
  }

  componentWillMount() {
    this.doFilter([])
  }

  componentWillReceiveProps(nextProps: Props<R>) {
    this.doFilter(nextProps.objects)
  }

  more() {
    this.setState({ limit: this.state.limit + 25 })
  }

  doFilter(objects: R[]) {
    this.setState({
      limit: 25,
      objects: objects.length ? objects : this.props.objects
    })
  }

  render() {
    const objects = this.state.objects.slice(0, this.state.limit)

    // TODO: the filter here may be confusing, should
    //   probably support searching by formula as well
    // TODO: add "infinite" scroll for paging
    return (
      <section className={this.props.name}>
        <Filter
          collection={this.props.objects}
          onChange={objs => this.doFilter(objs)}
          placeholder={`Filter ${this.props.name} by text`}
        />

        {objects.map((obj: R) => <this.props.component key={obj.uid} object={obj} />)}

        {this.props.objects.length > this.state.limit
          ? <button className="btn btn-default" onClick={() => this.more()}>Show More</button>
          : ''}
      </section>
    )
  }
}

export default List
