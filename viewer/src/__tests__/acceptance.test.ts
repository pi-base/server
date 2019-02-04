import puppeteer from 'puppeteer'

const headless = false

describe.skip('User acceptance tests', () => {
  jest.setTimeout(60 * 1000)

  let browser
  let page

  const click = async selector => {
    await page.waitForSelector(selector)
    return await page.click(selector)
  }

  const find = async selector => {
    await page.waitForSelector(selector)
    return await page.$(selector)
  }

  const findAll = async selector => {
    await page.waitForSelector(selector)
    return await page.$$(selector)
  }

  beforeAll(async () => {
    browser = await puppeteer.launch({ headless })
    page = await browser.newPage()

    page.emulate({
      viewport: {
        width: 500,
        height: 2400
      },
      userAgent: ''
    })

    page.on('error', err => { throw err })

    await page.goto('http://localhost:3000/')

    page.setDefaultTimeout(2 * 1000)
  })
  afterAll(async () => { browser.close() })

  it('loads the home page', async () => {
    await click('.jumbotron a')
    await click('.search-examples a')

    const el = await find('[data-test="search-results"] tr td')
    const text = await page.evaluate(e => e.textContent, el)

    expect(text).toEqual('Continuum-power of closed unit intervals')
  })
})