export const MASTER = 'master'

const PRODUCTION_GRAPH_URL = 'https://server.counterexamples.info'
const DEVELOPMENT_GRAPH_URL = process.env.REACT_APP_GRAPH_URL || 'http://localhost:3141'

export const GRAPH_URL = process.env.NODE_ENV === 'development' ? DEVELOPMENT_GRAPH_URL : PRODUCTION_GRAPH_URL
