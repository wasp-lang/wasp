import { useState, useCallback } from 'react';
import Box from '@mui/material/Box';
import Button from '@mui/material/Button';
import Container from '@mui/material/Container';
import Grid from '@mui/material/Grid';
import Typography from '@mui/material/Typography';
import Stack from '@mui/material/Stack';
import Paper from '@mui/material/Paper';
import Accordion from '@mui/material/Accordion';
import AccordionSummary from '@mui/material/AccordionSummary';
import AccordionDetails from '@mui/material/AccordionDetails';
import Divider from '@mui/material/Divider';
import RocketLaunchIcon from '@mui/icons-material/RocketLaunch';
import BrushIcon from '@mui/icons-material/Brush';
import CodeIcon from '@mui/icons-material/Code';
import SpeedIcon from '@mui/icons-material/Speed';
import ExpandMoreIcon from '@mui/icons-material/ExpandMore';
import PlayArrowIcon from '@mui/icons-material/PlayArrow';

// -- SEO head metadata for SSR --
export const head = () => ({
  title: 'MUI SSR Example | Wasp',
  meta: [
    {
      name: 'description',
      content:
        'A minimal landing page demonstrating MUI with server-side rendering in the Wasp framework.',
    },
    { property: 'og:title', content: 'MUI SSR Example | Wasp' },
  ],
});

// ---------------------------------------------------------------------------
// Hero Section
// ---------------------------------------------------------------------------

function HeroSection() {
  return (
    <Box
      component="section"
      sx={{
        py: { xs: 10, md: 15 },
        background: (theme) =>
          `linear-gradient(135deg, ${theme.palette.primary.darker} 0%, ${theme.palette.primary.dark} 50%, ${theme.palette.primary.main} 100%)`,
        color: 'common.white',
        position: 'relative',
        overflow: 'hidden',
      }}
    >
      {/* Decorative circles */}
      <Box
        sx={{
          position: 'absolute',
          width: 400,
          height: 400,
          borderRadius: '50%',
          bgcolor: 'rgba(255,255,255,0.05)',
          top: -100,
          right: -100,
        }}
      />
      <Box
        sx={{
          position: 'absolute',
          width: 250,
          height: 250,
          borderRadius: '50%',
          bgcolor: 'rgba(255,255,255,0.04)',
          bottom: -80,
          left: -40,
        }}
      />

      <Container maxWidth="lg">
        <Grid container spacing={4} alignItems="center">
          <Grid size={{ xs: 12, md: 6 }}>
            <Typography
              variant="overline"
              sx={{
                color: 'primary.lighter',
                mb: 2,
                display: 'block',
              }}
            >
              Wasp + MUI + SSR
            </Typography>

            <Typography variant="h1" sx={{ mb: 3 }}>
              Build fast, render faster
            </Typography>

            <Typography
              sx={{
                mb: 5,
                color: 'rgba(255,255,255,0.72)',
                maxWidth: 480,
                fontSize: '1.125rem',
                lineHeight: 1.7,
              }}
            >
              This example demonstrates server-side rendering with MUI and
              Emotion CSS-in-JS in a Wasp application. Styles are extracted on
              the server and injected into the HTML head — no flash of unstyled
              content.
            </Typography>

            <Stack direction="row" spacing={2} alignItems="center">
              <Button
                variant="contained"
                size="large"
                sx={{
                  bgcolor: 'common.white',
                  color: 'primary.dark',
                  '&:hover': { bgcolor: 'grey.100' },
                }}
              >
                Get Started
              </Button>

              <Button
                variant="outlined"
                size="large"
                startIcon={<PlayArrowIcon />}
                sx={{
                  color: 'common.white',
                  borderColor: 'rgba(255,255,255,0.4)',
                  '&:hover': { borderColor: 'common.white' },
                }}
              >
                See How It Works
              </Button>
            </Stack>
          </Grid>

          <Grid
            size={{ xs: 12, md: 6 }}
            sx={{ display: { xs: 'none', md: 'flex' }, justifyContent: 'center' }}
          >
            <Box
              sx={{
                width: 320,
                height: 320,
                borderRadius: 4,
                bgcolor: 'rgba(255,255,255,0.08)',
                border: '1px solid rgba(255,255,255,0.12)',
                display: 'flex',
                flexDirection: 'column',
                alignItems: 'center',
                justifyContent: 'center',
                gap: 2,
                backdropFilter: 'blur(12px)',
              }}
            >
              <CodeIcon sx={{ fontSize: 64, color: 'primary.lighter' }} />
              <Typography variant="h6" sx={{ color: 'common.white' }}>
                SSR + Emotion
              </Typography>
              <Typography
                variant="body2"
                sx={{ color: 'rgba(255,255,255,0.6)', textAlign: 'center', px: 3 }}
              >
                View page source to see server-rendered MUI styles in the
                &lt;head&gt;
              </Typography>
            </Box>
          </Grid>
        </Grid>
      </Container>
    </Box>
  );
}

// ---------------------------------------------------------------------------
// Services Section
// ---------------------------------------------------------------------------

const SERVICES = [
  {
    icon: <RocketLaunchIcon sx={{ fontSize: 48 }} />,
    name: 'Server-Side Rendering',
    description:
      'Pages are rendered on the server, delivering fully formed HTML with styles to the browser for instant first paint.',
    color: 'primary' as const,
  },
  {
    icon: <BrushIcon sx={{ fontSize: 48 }} />,
    name: 'CSS-in-JS Extraction',
    description:
      'Emotion styles are extracted during SSR and injected into the HTML head, eliminating flash of unstyled content.',
    color: 'secondary' as const,
  },
  {
    icon: <CodeIcon sx={{ fontSize: 48 }} />,
    name: 'MUI Components',
    description:
      'Full Material UI component library with custom theming — Typography, Grid, Paper, Accordion, and more.',
    color: 'info' as const,
  },
  {
    icon: <SpeedIcon sx={{ fontSize: 48 }} />,
    name: 'Seamless Hydration',
    description:
      'Server-rendered HTML seamlessly hydrates on the client with matching Emotion class names — no mismatches.',
    color: 'success' as const,
  },
];

function ServicesSection() {
  return (
    <Box component="section" sx={{ py: { xs: 8, md: 12 } }}>
      <Container maxWidth="lg">
        <Stack
          spacing={2}
          sx={{
            mb: 6,
            maxWidth: 520,
            mx: { xs: 'auto', md: 'unset' },
            textAlign: { xs: 'center', md: 'left' },
          }}
        >
          <Typography variant="overline" sx={{ color: 'text.disabled' }}>
            Features
          </Typography>

          <Typography variant="h2">What this example shows</Typography>

          <Typography sx={{ color: 'text.secondary' }}>
            A focused demonstration of Wasp's SSR capabilities with the MUI
            component library and Emotion CSS-in-JS.
          </Typography>
        </Stack>

        <Grid container spacing={4}>
          {SERVICES.map((service, index) => (
            <Grid key={service.name} size={{ xs: 12, sm: 6, md: 3 }}>
              <Paper
                variant="outlined"
                sx={(theme) => ({
                  px: 3,
                  py: 4,
                  height: '100%',
                  textAlign: 'center',
                  borderRadius: 2,
                  transition: 'box-shadow 0.3s, transform 0.3s',
                  '&:hover': {
                    transform: 'translateY(-4px)',
                    boxShadow: theme.shadows[8],
                  },
                  ...(index % 2 === 1 && {
                    [theme.breakpoints.up('md')]: {
                      py: 6,
                    },
                  }),
                })}
              >
                <Box sx={{ color: `${service.color}.main`, mb: 2 }}>
                  {service.icon}
                </Box>

                <Typography variant="h6" sx={{ mb: 1 }}>
                  {service.name}
                </Typography>

                <Typography variant="body2" sx={{ color: 'text.secondary' }}>
                  {service.description}
                </Typography>
              </Paper>
            </Grid>
          ))}
        </Grid>
      </Container>
    </Box>
  );
}

// ---------------------------------------------------------------------------
// FAQ Section
// ---------------------------------------------------------------------------

const FAQS = [
  {
    question: 'What is SSR in Wasp?',
    answer:
      'Server-side rendering in Wasp pre-renders your React pages on the server, delivering fully formed HTML to the browser. This improves initial load time, SEO, and prevents flash of unstyled content when used with CSS-in-JS libraries like Emotion.',
  },
  {
    question: 'How does Emotion CSS extraction work?',
    answer:
      'During SSR, Wasp looks for a src/ssr/styles.tsx file that exports a createSsrStylesProvider function. This function returns a Wrapper component (CacheProvider) and an extractStyles function that collects all generated CSS after rendering and injects it into the HTML <head>.',
  },
  {
    question: 'Do I need to configure anything special for MUI?',
    answer:
      'You need three things: (1) Install @emotion/cache and @emotion/server alongside MUI, (2) Create src/ssr/styles.tsx with the Emotion cache provider, and (3) Set ssr: true on your pages in main.wasp. Wasp handles the rest automatically.',
  },
  {
    question: 'Can I use other CSS-in-JS libraries?',
    answer:
      'Yes! The SSR styles provider hook is library-agnostic. You can implement createSsrStylesProvider for styled-components, Stitches, or any other CSS-in-JS library that supports SSR extraction. The interface just needs a Wrapper component and an extractStyles function.',
  },
  {
    question: 'How can I verify SSR is working?',
    answer:
      'View the page source (right-click > View Page Source). You should see: (1) Your page content already rendered in the HTML, (2) <style data-emotion="css ..."> tags in the <head> with your MUI styles, (3) No flash of unstyled content on page load.',
  },
];

function FaqSection() {
  const [expanded, setExpanded] = useState<string | false>(false);

  const handleChange = useCallback(
    (panel: string) => (_event: React.SyntheticEvent, isExpanded: boolean) => {
      setExpanded(isExpanded ? panel : false);
    },
    []
  );

  return (
    <Box
      component="section"
      sx={{ py: { xs: 8, md: 12 }, bgcolor: 'grey.100' }}
    >
      <Container maxWidth="lg">
        <Grid container spacing={4} alignItems="flex-start">
          <Grid size={{ xs: 12, md: 5 }}>
            <Typography variant="overline" sx={{ color: 'text.disabled', mb: 2, display: 'block' }}>
              FAQ
            </Typography>

            <Typography variant="h2" sx={{ mb: 2 }}>
              Frequently asked questions
            </Typography>

            <Typography sx={{ color: 'text.secondary' }}>
              Common questions about using MUI with server-side rendering in
              Wasp.
            </Typography>
          </Grid>

          <Grid size={{ xs: 12, md: 7 }}>
            {FAQS.map((faq) => (
              <Accordion
                key={faq.question}
                expanded={expanded === faq.question}
                onChange={handleChange(faq.question)}
                disableGutters
                sx={{
                  bgcolor: 'transparent',
                  boxShadow: 'none',
                  '&:before': { display: 'none' },
                  py: 0.5,
                }}
              >
                <AccordionSummary expandIcon={<ExpandMoreIcon />}>
                  <Typography variant="h6">{faq.question}</Typography>
                </AccordionSummary>
                <AccordionDetails>
                  <Typography sx={{ color: 'text.secondary' }}>
                    {faq.answer}
                  </Typography>
                </AccordionDetails>
              </Accordion>
            ))}
            <Divider />
          </Grid>
        </Grid>
      </Container>
    </Box>
  );
}

// ---------------------------------------------------------------------------
// Landing Page
// ---------------------------------------------------------------------------

export function LandingPage() {
  return (
    <>
      <HeroSection />
      <ServicesSection />
      <FaqSection />
    </>
  );
}
