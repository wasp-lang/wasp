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

      brand: '$waspYellow',
      brandAccent: '#ffdb46',

      submitButtonText: 'black'

    },
    fontSizes: {
      sm: '0.875rem'
    }
  }
})
