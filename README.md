# π-Base Server

A GraphQL server for querying and updating the [π-base data repository](https://github.com/pi-base/data)

[![CircleCI](https://circleci.com/gh/pi-base/server.svg?style=svg)](https://circleci.com/gh/pi-base/server)
[![Docker Repository on Quay](https://quay.io/repository/jamesdabbs/pibase/status "Docker Repository on Quay")](https://quay.io/repository/jamesdabbs/pibase)

## Installation

To run a server locally, `git clone` and `cd` in to the project and then

```
$ stack setup # to install the appropriate version of GHC
$ stack build # to build the server
```

Once built, you can also run `stack exec -- pibase --help` to view other cli options.

The server operates on a local copy of the [data repository](https://github.com/pi-base/data). By default, it will clone a copy from upstream - run `stack exec -- pibase settings` and inspect the `repoPath` and `upstream` settings to see where to and from.

Assuming everything looks good, you can run

```
$ stack exec -- pibase server # to start the server
```

and should see a message like `[Info] Application starting on port 3141`. You should be able to visit `http://localhost:3141` in your browser and see metadata about the running server.

## Running the Viewer

See [the Viewer README](https://github.com/pi-base/server/tree/master/viewer#installation) for details about running the frontend viewer.

## Questions

We aim to keep this documentation as up-to-date as possible, but if you ever have any questions, please feel free to reach out - either by email or by opening a ticket on Github.
