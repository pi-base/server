import * as React from 'react'

import { CitationType } from '../types'

const CitationDisplay = ({ citation = '' }) => {
  const [type, ref, ..._] = citation.split(':')
  let url
  switch (type as CitationType) {
    case 'doi':
      return <code>{ref}</code>
    case 'mr':
      url = `https://mathscinet.ams.org/mathscinet-getitem?mr=${ref}`
      return <a href={url} target="_blank">Math Reviews - {ref}</a>
    case 'wikipedia':
      url = `https://en.wikipedia.org/wiki/${ref}`
      return <a href={url} target="_blank">Wikipedia - {ref}</a>
    default:
      return <code>{type}:{ref}</code>
  }
}

export default CitationDisplay