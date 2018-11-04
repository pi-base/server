'use strict'

const fetch = require('isomorphic-fetch')
const fs = require('fs')

const path = './test/fixtures/viewer.json'

fetch('http://localhost:3141/graphql', {
  'headers': {
    'accept': 'application/json',
    'content-type': 'application/json',
    'pragma': 'no-cache'
  },
  'body': JSON.stringify({
    'query': `
      query {
        viewer {
          version
          spaces {
            uid
            name
            aliases
            traits {
              property {
                uid
              }
              value
            }
          }
          properties {
            uid
            name
          }
          theorems {
            uid
            if
            then
          }
        }
      }
    `
  }),
  'method': 'POST'
}).then(r =>
  r.json()
).then(res => {
  console.log('Writing results to', path)
  fs.writeFileSync(path, JSON.stringify(res.data, null, 2))
})
