import * as React from 'react'

type Example = { name: string, query: string }

interface Props {
  onSelect?: (example: string) => void
}

const examples: Example[] = [
  {
    name: 'All Non-Metric Continua',
    query: 'compact + connected + t_2 + ~metrizable'
  },
  {
    name: 'A Common Non-Theorem',
    query: 'first countable + separable + ~second countable'
  }
]

const Example = ({ example, onClick }: { example: Example, onClick: () => void }) => (
  <article>
    <h5>{example.name}</h5>
    <a onClick={onClick}>
      <pre>{example.query}</pre>
    </a>
  </article>
)

// TODO: better passthrough props
const Examples = ({ onSelect, ...rest }: Props & Partial<any>) => {
  return (
    <div {...rest}>
      <p>Not sure where to start? Try one of the following searches</p>
      {examples.map(example =>
        <Example key={example.name} example={example} onClick={() => onSelect && onSelect(example.query)} />
      )}
    </div>
  )
}

export default Examples
