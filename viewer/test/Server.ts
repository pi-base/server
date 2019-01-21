import child_process, { ChildProcess } from 'child_process'

type Opts = Partial<{
  port: number
  bin: string
}>

class Server {
  _bin: string
  _host: string
  _port: number
  _process: ChildProcess | null
  _tracing: boolean

  constructor({ port, bin }: Opts = {}) {
    this._port = port || 3141
    this._host = `http://localhost:${this._port}`

    this._bin = (
      bin ||
      process.env.SERVER_BIN_PATH ||
      child_process.execSync('stack exec -- which pibase').toString('utf8')
    ).trim()

    this._process = null
    this._tracing = process.env.SERVER_TRACING ? true : false
  }

  get host() { return this._host }

  boot(): Promise<void> {
    if (this._process) { return Promise.resolve() }

    this.trace(`Booting server on ${this._port}`)

    return new Promise(resolve => {
      this._process = child_process.exec(
        `${this._bin.trim()} server --test --port ${this._port}`,
        {
          cwd: '..',
          env: process.env
        },
      )

      this._process!.stdout.on('data', data => {
        this.trace(data)
        if (data.includes('starting')) { resolve() }
      })
      this._process!.stderr.on('data', data => {
        this.trace(data)
      })
    })
  }

  async shutdown() {
    if (!this._process) { return }
    this._process.kill()
    this._process = null
  }

  trace(...args) {
    if (!this._tracing) { return }
    console.info('[Server]', ...args)
  }
}

export default Server