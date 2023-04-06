import { createStitches } from '@stitches/react'

export const {
  styled,
  css
} = createStitches({
  theme: {
    colors: {
      waspYellow: '#ffcc00',
      gray700: '#a1a5ab',
      gray600: '#d1d5db',
      gray500: 'gainsboro',
      gray400: '#f0f0f0',
      red: '#FED7D7',
      green: '#C6F6D5',

      brand: '$waspYellow',
      brandAccent: '#ffdb46',
      errorBackground: '$red',
      errorText: '#2D3748',
      successBackground: '$green',
      successText: '#2D3748',

      submitButtonText: 'black',

    },
    fontSizes: {
      sm: '0.875rem'
    }
  }
})
