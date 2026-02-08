/**
 * Simplified typography adapted from the MUI Zone template.
 * Uses Barlow for headings and Inter for body text.
 */
const primaryFont = '"Inter", -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif';
const secondaryFont = '"Barlow", -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif';

export const typography = {
  fontFamily: primaryFont,

  h1: {
    fontFamily: secondaryFont,
    fontWeight: 800,
    fontSize: '2.5rem',
    lineHeight: 1.25,
    '@media (min-width:600px)': {
      fontSize: '3rem',
    },
    '@media (min-width:900px)': {
      fontSize: '3.5rem',
    },
  },
  h2: {
    fontFamily: secondaryFont,
    fontWeight: 800,
    fontSize: '2rem',
    lineHeight: 1.3,
    '@media (min-width:600px)': {
      fontSize: '2.25rem',
    },
    '@media (min-width:900px)': {
      fontSize: '2.75rem',
    },
  },
  h3: {
    fontFamily: secondaryFont,
    fontWeight: 700,
    fontSize: '1.5rem',
    lineHeight: 1.4,
    '@media (min-width:900px)': {
      fontSize: '2rem',
    },
  },
  h4: {
    fontFamily: secondaryFont,
    fontWeight: 700,
    fontSize: '1.25rem',
    lineHeight: 1.5,
    '@media (min-width:900px)': {
      fontSize: '1.5rem',
    },
  },
  h5: {
    fontFamily: secondaryFont,
    fontWeight: 700,
    fontSize: '1.125rem',
    lineHeight: 1.5,
  },
  h6: {
    fontFamily: secondaryFont,
    fontWeight: 700,
    fontSize: '1rem',
    lineHeight: 1.5,
  },
  subtitle1: {
    fontWeight: 600,
    fontSize: '1rem',
    lineHeight: 1.5,
  },
  subtitle2: {
    fontWeight: 600,
    fontSize: '0.875rem',
    lineHeight: 1.57,
  },
  body1: {
    fontSize: '1rem',
    lineHeight: 1.5,
  },
  body2: {
    fontSize: '0.875rem',
    lineHeight: 1.57,
  },
  caption: {
    fontSize: '0.75rem',
    lineHeight: 1.5,
  },
  overline: {
    fontSize: '0.75rem',
    fontWeight: 700,
    lineHeight: 1.5,
    textTransform: 'uppercase' as const,
    letterSpacing: '1.1px',
  },
  button: {
    fontWeight: 700,
    fontSize: '0.875rem',
    lineHeight: 1.71,
    textTransform: 'unset' as const,
  },
};
