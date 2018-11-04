import * as React from 'react'

let explorer = graph => props => <div />

if (process.env.NODE_ENV === 'development') {
  // FIXME
  // explorer = graph => {
  //   const GraphiQL = require('graphiql')

  //   const fetcher = (params) => {
  //     // TODO: read URL from graph object
  //     return fetch(GRAPHQL_SERVER_URL + '/graphql', {
  //       method: 'post',
  //       headers: { 'Content-Type': 'application/json' },
  //       body: JSON.stringify(params)
  //     }).then(res => res.json())
  //   }

  //   // TODO: load schema from disk
  //   return props => <GraphiQL fetcher={fetcher} schema={null} />
  // }
}

export default explorer