import React from 'react';

// TODO: Should I take this out of auth/, into more general place?
export const DefaultPageLoader = ({status, error}) => {
  switch (status) {
    case 'loading':
      return null;
    case 'error':
    // TODO: Should I rather throw this error and have some error boundary cach it (also add that then)?
    // Or just have nicer error message here, more mindful of handling possible errors?
      return <span>Error: {error?.message || error}</span>;
  }
}
