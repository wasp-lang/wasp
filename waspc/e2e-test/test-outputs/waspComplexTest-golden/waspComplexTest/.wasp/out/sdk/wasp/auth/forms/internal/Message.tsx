import type { ComponentProps } from 'react'
import styles from './Message.module.css'
import './auth-styles.css'
import { clsx } from './util'

// PRIVATE API
export const Message = ({ children, className, ...props }: ComponentProps<'div'>) => (
  <div className={clsx(styles.message, className)} {...props}>
    {children}
  </div>
)

// PRIVATE API
export const MessageError = ({ children, className, ...props }: ComponentProps<'div'>) => (
  <div className={clsx(styles.messageError, className)} {...props}>
    {children}
  </div>
)

// PRIVATE API
export const MessageSuccess = ({ children, className, ...props }: ComponentProps<'div'>) => (
  <div className={clsx(styles.messageSuccess, className)} {...props}>
    {children}
  </div>
)
