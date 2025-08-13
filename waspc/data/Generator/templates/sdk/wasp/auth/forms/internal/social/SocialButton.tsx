import type { ComponentProps } from 'react'
import '../auth-styles.css'
import { clsx } from '../util'
import styles from './SocialButton.module.css'

// PRIVATE API
export const SocialButton = ({ children, className, ...props }: ComponentProps<'a'>) => (
  <a className={clsx(styles.socialButton, className)} {...props}>
    {children}
  </a>
)
