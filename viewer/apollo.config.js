module.exports = {
  client: {
    service: 'pi-base',
    localSchemaFile: __dirname + '/src/graph/schema.gql',
    includes: [
      __dirname + '/src/graph/queries/**/*.gql',
      __dirname + '/src/graph/mutations/**/*.gql'
    ],
  },
};