import { createTheme } from '@mui/material/styles';
import { palette } from './palette';
import { typography } from './typography';

export const theme = createTheme({
  palette,
  typography,
  shape: {
    borderRadius: 8,
  },
  components: {
    MuiCssBaseline: {
      styleOverrides: {
        '*': {
          boxSizing: 'border-box',
        },
        html: {
          scrollBehavior: 'smooth',
        },
        body: {
          WebkitFontSmoothing: 'antialiased',
          MozOsxFontSmoothing: 'grayscale',
        },
      },
    },
    MuiButton: {
      defaultProps: {
        disableElevation: true,
      },
      styleOverrides: {
        root: {
          borderRadius: 8,
          textTransform: 'none',
          fontWeight: 700,
        },
        sizeLarge: {
          padding: '11px 24px',
          fontSize: '0.9375rem',
        },
      },
    },
    MuiPaper: {
      defaultProps: {
        elevation: 0,
      },
    },
    MuiAppBar: {
      defaultProps: {
        elevation: 0,
      },
    },
  },
});
