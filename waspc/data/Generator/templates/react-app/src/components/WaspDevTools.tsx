{{={= =}=}}
import styles from './WaspDevTools.module.css'

export function WaspDevTools() {
  return (
    <div className={styles.container}>
      <div className={styles.devTools}>
        Prisma status: <PrismaStatusText status="{= prismaStatus =}" />
      </div>
    </div>
  )
}

type PrismaStatus = 'ok' | 'needsMigration' | 'error'

const statusToText: Record<PrismaStatus, string> = {
  ok: 'all good',
  needsMigration: 'needs migration',
  error: 'error',
}

const statusToColor: Record<PrismaStatus, string> = {
  ok: '#22c55e',
  needsMigration: '#a855f7',
  error: '#dc2626',
}

function PrismaStatusText({ status }: { status: PrismaStatus }) {
  const color = statusToColor[status]
  const text = statusToText[status]

  return (
    <span
      className={styles.prismaStatusText}
      style={{ color }}
    >
      {text}{' '}
      <svg width="10" height="10" style={{ marginLeft: '0.5rem' }}>
        <circle cx="5" cy="5" r="5" fill="currentColor" />
      </svg>
    </span>
  )
}
