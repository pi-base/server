# π-Base Viewer

# Installation

## Setting up the server

The viewer will need a running backend server. Your options are:

* Install and run the server locally
* Run a server using docker (TODO: document)
* Use a live server (e.g. `https://server.counterexamples.info`, `https://server.staging.counterexamples.info`)

In any case, note the url of the running server for future reference.

## Installing node

We recommend installing `nvm` to manage `node` versions. We are currently developing against `lts/carbon`, and intend to track the most recent lts.

* Install NVM using the script provided at https://github.com/creationix/nvm#installation
* Run `nvm install lts/carbon` to install
* Run `nvm use lts/carbon` to set the default version of node

## Installing yarn

We use `yarn` as a package manager to ensure consistent builds. You can install
yarn by following the directions at https://yarnpkg.com/lang/en/docs/install/

## Running the viewer

Once the dependencies are installed and a server is running, `cd` into the `viewer` directory and run

    yarn install
    yarn run schema
    REACT_APP_GRAPH_URL=<Server URL> yarn start

If everything worked, `http://localhost:3000` should open a copy of the viewer running against the configured server.