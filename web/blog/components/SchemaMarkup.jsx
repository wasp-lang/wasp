import React from 'react';
import Head from '@docusaurus/Head';

export default function SchemaMarkup({ schema }) {
  return (
    <Head>
      <script type="application/ld+json">
        {JSON.stringify(schema)}
      </script>
    </Head>
  );
}
