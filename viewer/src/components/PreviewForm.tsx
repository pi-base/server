import * as F from 'formik'
import * as React from 'react'
import { useRef } from 'react'

interface PreviewFormProps<Values, Result> {
  // FIXME: sort out these component types
  Preview: any
  Fields: any
  initialValues: Values
  validate: (values: Values) => {
    result?: Result
    errors: F.FormikErrors<Values>
  }
  onSubmit: (result: Result) => any
}

const noErrors = (errors: F.FormikErrors<any>): boolean =>
  Object.keys(errors).length === 0

function PreviewForm<Values, Result>(
  props: PreviewFormProps<Values, Result>
) {
  const {
    Preview,
    Fields,
    initialValues,
    validate,
    onSubmit,
    ...rest
  } = props
  const preview = useRef<Result | undefined>(undefined)

  const handleValidate = (values: Values) => {
    const { result, errors } = validate(values)
    if (result) { preview.current = result }
    return errors
  }

  handleValidate(initialValues)

  const handleSubmit = (values: Values) => {
    const { result, errors } = validate(values)
    if (noErrors(errors)) { onSubmit(result!) }
  }

  return (
    <F.Formik
      initialValues={initialValues}
      onSubmit={handleSubmit}
      validate={handleValidate}
      validateOnBlur={true}
      render={(props: F.FormikProps<Values>) => {
        const { isSubmitting } = props

        return (
          <div className="row">
            <F.Form className="col-md-6">
              <Fields />
              <button
                className="btn btn-default"
                type="submit"
                disabled={isSubmitting}
              >
                Save
              </button>
            </F.Form>
            <div className="col-md-6">
              {preview.current && <Preview result={preview.current} />}
            </div>
          </div>
        )
      }}
      {...rest}
    />
  )
}

export default PreviewForm