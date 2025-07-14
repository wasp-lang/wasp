import React, { useState, useRef } from 'react';
import CodeBlock from '@theme/CodeBlock';

interface Props {
  /** Name of the environment variable to show, e.g. "JWT_SECRET" */
  varName?: string;
  /** Label shown on the button before generation */
  buttonLabel?: string;
}

export function SecretGeneratorBlock({ varName = 'JWT_SECRET', buttonLabel = 'Generate Secret' }: Props) {
  const [secret, setSecret] = useState<string | null>(null);
  const timeoutRef = useRef<NodeJS.Timeout | null>(null);

  const generate = () => {
    const bytes = crypto.getRandomValues(new Uint8Array(32));
    const hex = Array.from(bytes, (b) => b.toString(16).padStart(2, '0')).join('');
    setSecret(hex);
    navigator.clipboard?.writeText(hex).catch(() => {});

    if (timeoutRef.current) clearTimeout(timeoutRef.current);
    timeoutRef.current = setTimeout(() => setSecret(null), 8000);
  };

  return (
    <div style={{ marginTop: 8, marginBottom: 16 }}>
      <CodeBlock language="env">{`${varName}=${secret ?? ''}`}</CodeBlock>
      <button
        onClick={generate}
        disabled={!!secret}
        style={{
          marginTop: 8,
          padding: '8px 16px',
          fontSize: '0.95rem',
          borderRadius: 4,
        }}
      >
        {secret ? 'Generated!' : buttonLabel}
      </button>
    </div>
  );
}

export default SecretGeneratorBlock; 