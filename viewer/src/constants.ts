type Stage = 'development' | 'staging' | 'production'

const PRODUCTION_VIEWER_URLS = [
  'topology.pi-base.org',
  'counterexamples.info'
]

let stage: Stage
if (process.env.NODE_ENV === 'production') {
  if (window && PRODUCTION_VIEWER_URLS.indexOf(window.location.hostname) !== -1) {
    stage = 'production'
  } else {
    stage = 'staging'
  }
} else {
  stage = 'development'
}

export const GRAPH_URLS = {
  production: 'https://server.counterexamples.info',
  staging: 'https://server.staging.counterexamples.info',
  development: 'http://localhost:3141'
}

export const STAGE = stage
export const GRAPH_URL = process.env.REACT_APP_GRAPH_URL || GRAPH_URLS[stage]
export const MASTER = 'master'
