import * as F from './Formula'

const f: F.Formula<string> = F.parse('compact + (connected || not second countable) + ~first countable')!

describe('parsing', () => {
  it('can parse a simple formula', () => {
    expect(
      F.parse('Compact')
    ).toEqual(
      F.atom('Compact', true)
    )
  })

  it('handles whitespace', () => {
    expect(
      F.parse('   \t   Second Countable \n ')
    ).toEqual(
      F.atom('Second Countable', true)
    )
  })

  it('can negate properties', () => {
    expect(
      F.parse('not compact')
    ).toEqual(
      F.atom('compact', false)
    )
  })

  it('inserts parens', () => {
    expect(
      F.parse('compact + connected + ~t_2')
    ).toEqual(
      F.and(
        F.atom('compact', true),
        F.atom('connected', true),
        F.atom('t_2', false)
      )
    )
  })

  it('allows parens', () => {
    expect(F.parse('(foo + bar)')).toEqual(
      F.and(
        F.atom('foo', true),
        F.atom('bar', true)
      )
    )
  })

  it('handles errors with parens', () => {
    expect(F.parse('(some stuff + |)')).toBeUndefined()
  })

  it('can parse nested formulae', () => {
    expect(f).toEqual(
      F.and(
        F.atom('compact', true),
        F.or(
          F.atom('connected', true),
          F.atom('second countable', false)
        ),
        F.atom('first countable', false)
      )
    )
  })

  it('handles empty strings', () => {
    expect(F.parse()).toBeUndefined()
    expect(F.parse('')).toBeUndefined()
  })
})

describe('mapping', () => {
  it('can map over atoms', () => {
    expect(
      F.map(atom => ({
        ...atom,
        property: atom.property.length,
        value: !atom.value
      }), f)
    ).toEqual(
      F.and(
        F.atom(7, false),
        F.or(
          F.atom(9, false),
          F.atom(16, true)
        ),
        F.atom(15, true)
      )
    )
  })

  it('accumulate property lists', () => {
    expect(F.properties(f)).toEqual(new Set([
      'compact', 'connected', 'second countable', 'first countable'
    ]))
  })

  it('can negate', () => {
    expect(F.negate(f)).toEqual(
      F.or(
        F.atom('compact', false),
        F.and(
          F.atom('connected', false),
          F.atom('second countable', true)
        ),
        F.atom('first countable', true)
      )
    )
  })

  it('can map over properties', () => {
    expect(F.mapProperty(p => p[0], f)).toEqual(
      F.and(
        F.atom('c', true),
        F.or(
          F.atom('c', true),
          F.atom('s', false)
        ),
        F.atom('f', false)
      )
    )
  })
})

describe('evaluation', () => {
  it('can find a match', () => {
    const traits = new Map()
    traits.set('compact', true)
    traits.set('second countable', false)
    traits.set('first countable', false)
    expect(F.evaluate(f, traits)).toEqual(true)
  })

  it('can find a miss', () => {
    const traits = new Map()
    traits.set('compact', true)
    traits.set('second countable', false)
    traits.set('first countable', true)
    expect(F.evaluate(f, traits)).toEqual(false)
  })

  it('can be undefined', () => {
    const traits = new Map()
    traits.set('compact', true)
    traits.set('second countable', true)
    traits.set('first countable', false)
    expect(F.evaluate(f, traits)).toBeUndefined()
  })
})