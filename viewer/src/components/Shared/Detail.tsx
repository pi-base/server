import * as React from 'react'

import { Citation } from '../../types'
import EditLink from '../Form/EditLink'
import Markdown from '../Markdown'
import References from '../References'
import Tex from '../Tex'
import { withRouter } from 'react-router'

interface Detailable {
  description: string
  references: Citation[]
}

type Props<T extends Detailable> = {
  object: T
  children: React.ReactNode
  editable?: boolean
}

const Edit = withRouter(({ match }) => (
  <EditLink to={match.url + '/edit'} className="btn btn-default btn-xs">
    Edit
  </EditLink>
))

const Detail = <T extends Detailable>({ object, editable = true, children }: Props<T>) => (
  <div>
    <h1>
      {children}
      {editable && <Edit />}
    </h1>
    <Tex><Markdown text={object.description} /></Tex>

    <References references={object.references} />
  </div>
)

export default Detail