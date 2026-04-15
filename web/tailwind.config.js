/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["./src/**/*.{js,jsx,ts,tsx}"],
  important: true,
  corePlugins: {
    preflight: false,
  },
  theme: {
    container: {
      // Tailwind defaults + custom 2xl to match wider layout (like fly.io/blog)
      screens: {
        sm: "640px",
        md: "768px",
        lg: "1024px",
        xl: "1280px",
        "2xl": "1400px",
      },
    },
    extend: {
      // NOTE(matija): Fonts are defined in two places due to Tailwind + Infima (Docusaurus) coexisting.
      // This config controls body text globally (via preflight.css's `html` rule) and Tailwind utilities.
      //
      // Heading fonts for docs/blog are controlled separately by Infima's --ifm-heading-font-family
      // in src/css/custom.css — update both when changing fonts.
      fontFamily: {
        sans: ["'IBM Plex Mono'", "monospace"],
        mono: ["'JetBrains Mono'", "monospace"],
        body: ["'IBM Plex Mono'", "monospace"],
      },
      colors: {
        wasp: {
          yellow: '#F5C842',
          'yellow-light': '#FFF3CC',
          'yellow-dark': '#D4A930',
          black: '#111',
          bg: '#F7F5F0',
          white: '#FAFAFA',
          g1: '#EEE',
          g2: '#DDD',
          g3: '#BBB',
          g4: '#999',
          g5: '#777',
          g6: '#555',
          g7: '#333',
        },
      },
    },
  },
  plugins: [],
};
