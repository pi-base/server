import { Store } from 'redux'
import { State } from '../reducers'
import { toggleDebug } from '../actions'
import { STAGE } from '../constants'
import Api from '../graph/Client/Api'

class Debug {
  _store: Store
  _graph: Api

  constructor({ store, graph }: { store: Store, graph: Api }) {
    this._store = store
    this._graph = graph
  }

  get state(): State {
    return this._store.getState()
  }

  get graph(): Api {
    return this._graph
  }

  set debug(on: boolean | undefined) {
    this._store.dispatch(toggleDebug(on))
  }

  get stage() {
    return STAGE
  }

  error() {
    // tslint:disable-next-line no-any
    ('' as any).floop('')
  }
}

export default Debug