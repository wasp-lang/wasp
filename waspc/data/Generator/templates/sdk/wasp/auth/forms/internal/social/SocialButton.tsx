import type { ComponentProps } from 'react'
import '../auth-styles.css'
import styles from './SocialButton.module.css'

// PRIVATE API
export const SocialButton = ({ children, className, ...props }: ComponentProps<'a'>) => (
  <a className={`${styles.socialButton} ${className || ""}`} {...props}>
    {children}
  </a>
)
