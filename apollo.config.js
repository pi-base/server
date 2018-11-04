module.exports = {
  client: {
    service: 'pi-base',
    localSchemaFile: 'graph/schema.gql',
    includes: [
      'graph/queries/**/*.gql',
      'graph/mutations/**/*.gql'
    ],
  },
};