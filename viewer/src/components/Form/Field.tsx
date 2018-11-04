import * as React from 'react'
import * as F from 'formik'

import Control, { Props } from './Control'

const Field = (props: Props) => {
  const { name } = props

  return (
    <F.Field
      name={name}
      render={({ field, form }: F.FieldProps<any>) => {
        return <Control
          {...props}
          {...field}
          errors={form.touched[name] ? form.errors[name] : undefined}
        />
      }}
    />
  )
}

export default Field
