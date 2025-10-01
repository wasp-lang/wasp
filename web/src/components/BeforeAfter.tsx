import React from 'react';
import CodeBlock from '@theme/CodeBlock';

interface BeforeAfterProps {
  children: React.ReactNode;
}

export default function BeforeAfter({ children }: BeforeAfterProps) {
  const childArray = React.Children.toArray(children);

  if (childArray.length !== 2) {
    return <>{children}</>;
  }

  const beforeChild = childArray[0];
  const afterChild = childArray[1];

  return (
    <div style={{
      display: 'grid',
      gridTemplateColumns: '1fr 1fr',
      gap: '1rem',
      margin: '1rem 0'
    }}>
      <div>
        <div style={{
          backgroundColor: '#ffeaa7',
          padding: '0.5rem',
          textAlign: 'center',
          fontWeight: 'bold',
          borderTop: '2px solid #fdcb6e',
          borderLeft: '2px solid #fdcb6e',
          borderRight: '2px solid #fdcb6e'
        }}>
          Before
        </div>
        <div style={{
          border: '2px solid #fdcb6e',
          borderTop: 'none'
        }}>
          {beforeChild}
        </div>
      </div>

      <div>
        <div style={{
          backgroundColor: '#55efc4',
          padding: '0.5rem',
          textAlign: 'center',
          fontWeight: 'bold',
          borderTop: '2px solid #00b894',
          borderLeft: '2px solid #00b894',
          borderRight: '2px solid #00b894'
        }}>
          After
        </div>
        <div style={{
          border: '2px solid #00b894',
          borderTop: 'none'
        }}>
          {afterChild}
        </div>
      </div>
    </div>
  );
}
