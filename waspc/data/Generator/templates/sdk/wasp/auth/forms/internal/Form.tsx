import type { ComponentProps } from 'react'
import styles from './Form.module.css'
import './auth-styles.css'
import { clsx } from './util'

// PRIVATE API
export const Form = ({ children, className, ...props }: ComponentProps<'form'>) => (
  <form className={clsx(styles.form, className)} {...props}>
    {children}
  </form>
)

// PUBLIC API
export const FormItemGroup = ({ children, className, ...props }: ComponentProps<'div'>) => (
  <div className={clsx(styles.formItemGroup, className)} {...props}>
    {children}
  </div>
)

// PUBLIC API
export const FormLabel = ({ children, className, ...props }: ComponentProps<'label'>) => (
  <label className={clsx(styles.formLabel, className)} {...props}>
    {children}
  </label>
)

// PUBLIC API
export const FormInput = ({ className, ...props }: ComponentProps<'input'>) => (
  <input className={clsx(styles.formInput, className)} {...props} />
)

// PUBLIC API
export const FormTextarea = ({ className, ...props }: ComponentProps<'textarea'>) => (
  <textarea className={clsx(styles.formTextarea, className)} {...props} />
)

// PUBLIC API
export const FormError = ({ children, className, ...props }: ComponentProps<'div'>) => (
  <div className={clsx(styles.formError, className)} {...props}>
    {children}
  </div>
)

// PRIVATE API
export const SubmitButton = ({ children, className, ...props }: ComponentProps<'button'>) => (
  <button className={clsx(styles.submitButton, className)} {...props}>
    {children}
  </button>
)
