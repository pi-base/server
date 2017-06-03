# π-Base Server

A GraphQL server for querying and updating the [π-base data repository](https://github.com/pi-base/data)

## Setup

To run a server locally, `git clone` and `cd` in to the project and then

```
$ stack setup               # to install the appropriate version of GHC
$ stack build yesod-bin     # to install the `yesod devel` helper
$ stack exec -- yesod devel # to start the server in development mode on port 3000
```

You may need to install some system libraries for Yesod; see [their install documentation for more](https://www.yesodweb.com/page/quickstart) if those build steps fail.

You will need a local copy of the [data repository](https://github.com/pi-base/data). If the default paths and settings in `config/settings.yml` don't match yours, you can override them by setting the corresponding environment variables before running the server:

```
$ export REPO_PATH=/my/path
$ stack exec -- yesod devel
```

If everything is properly configured, you should be able to visit `http://localhost:3000/viewer` to see the parsed viewer JSON or make GraphQL queries to `http://localhost:3000/graphql`.
