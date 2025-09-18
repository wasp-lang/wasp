import styles from './Loader.module.css'

// Spans a accross the whole screen and displays a spinner in the very middle.
export function Loader() {
  const loaderClassName = ['wasp-loader', styles.loader].join(' ')
  return (
    <div className={loaderClassName}>
      <div className={styles.loaderCircle} />
      <div className={styles.loaderAccessibilityText}>Loading...</div>
    </div>
  )
}
