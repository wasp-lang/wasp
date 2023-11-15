/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ['./src/**/*.{js,ts,jsx,tsx}'],
  theme: {
    extend: {
      skew: {
        'min2': '-2deg',
        'min4': '-4deg',
        'min6': '-6deg',
      }
    },
    focus: {
      outline: 'none',
    },
  },
  plugins: [
    // require('@tailwindcss/aspect-ratio')
  ],
};
