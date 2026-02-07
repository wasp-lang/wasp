import Box from '@mui/material/Box';
import Button from '@mui/material/Button';
import Container from '@mui/material/Container';
import Divider from '@mui/material/Divider';
import Grid from '@mui/material/Grid';
import Typography from '@mui/material/Typography';
import Stack from '@mui/material/Stack';
import ArrowForwardIcon from '@mui/icons-material/ArrowForward';
import GroupsIcon from '@mui/icons-material/Groups';
import PublicIcon from '@mui/icons-material/Public';
import WorkspacePremiumIcon from '@mui/icons-material/WorkspacePremium';

// -- SEO head metadata for SSR --
export const head = () => ({
  title: 'About | MUI SSR Example',
  meta: [
    {
      name: 'description',
      content:
        'Learn about Wasp SSR with MUI — server-side rendered pages with full CSS-in-JS support.',
    },
    { property: 'og:title', content: 'About | MUI SSR Example' },
  ],
});

// ---------------------------------------------------------------------------
// Stats data
// ---------------------------------------------------------------------------

const STATS = [
  {
    icon: <GroupsIcon sx={{ fontSize: 40, color: 'primary.main' }} />,
    value: '10,000+',
    label: 'Developers',
    description:
      'Wasp is trusted by thousands of developers building full-stack web applications with ease.',
  },
  {
    icon: <PublicIcon sx={{ fontSize: 40, color: 'secondary.main' }} />,
    value: '50+',
    label: 'Countries',
    description:
      'Our community spans the globe, with contributors and users on every continent.',
  },
  {
    icon: <WorkspacePremiumIcon sx={{ fontSize: 40, color: 'success.main' }} />,
    value: '99.9%',
    label: 'Uptime',
    description:
      'Built on proven technologies — React, Node.js, and Prisma — for reliability you can count on.',
  },
];

// ---------------------------------------------------------------------------
// About Hero
// ---------------------------------------------------------------------------

function AboutHero() {
  return (
    <Box
      component="section"
      sx={{
        py: { xs: 10, md: 15 },
        background: (theme) =>
          `linear-gradient(135deg, ${theme.palette.grey[900]} 0%, ${theme.palette.grey[800]} 100%)`,
        color: 'common.white',
      }}
    >
      <Container maxWidth="lg">
        <Grid container spacing={4} alignItems="center">
          <Grid size={{ xs: 12, md: 6 }}>
            <Typography variant="overline" sx={{ color: 'grey.500', mb: 2, display: 'block' }}>
              About Us
            </Typography>

            <Typography variant="h1" sx={{ mb: 3 }}>
              Who we are
            </Typography>

            <Typography
              sx={{
                color: 'grey.400',
                fontSize: '1.125rem',
                lineHeight: 1.7,
                maxWidth: 480,
              }}
            >
              This is the about page of the MUI SSR example. It demonstrates
              that server-side rendering works consistently across multiple
              pages, each with its own SEO metadata and fully styled content
              delivered in the initial HTML response.
            </Typography>

            <Button
              size="large"
              endIcon={<ArrowForwardIcon />}
              sx={{
                mt: 4,
                color: 'common.white',
                borderColor: 'rgba(255,255,255,0.3)',
                '&:hover': { borderColor: 'common.white' },
              }}
              variant="outlined"
            >
              Learn More
            </Button>
          </Grid>

          <Grid
            size={{ xs: 12, md: 6 }}
            sx={{
              display: 'flex',
              justifyContent: 'center',
            }}
          >
            <Box
              sx={{
                width: 280,
                height: 280,
                borderRadius: '50%',
                background: (theme) =>
                  `linear-gradient(135deg, ${theme.palette.primary.dark} 0%, ${theme.palette.secondary.dark} 100%)`,
                display: 'flex',
                flexDirection: 'column',
                alignItems: 'center',
                justifyContent: 'center',
                gap: 1,
              }}
            >
              <Typography variant="h2" sx={{ color: 'common.white' }}>
                SSR
              </Typography>
              <Typography variant="body2" sx={{ color: 'rgba(255,255,255,0.7)' }}>
                Server-Side Rendered
              </Typography>
            </Box>
          </Grid>
        </Grid>
      </Container>
    </Box>
  );
}

// ---------------------------------------------------------------------------
// Stats Section
// ---------------------------------------------------------------------------

function StatsSection() {
  return (
    <Box component="section" sx={{ py: { xs: 8, md: 12 } }}>
      <Container maxWidth="lg">
        <Box sx={{ textAlign: 'center', mb: 8 }}>
          <Typography variant="overline" sx={{ color: 'text.disabled', mb: 2, display: 'block' }}>
            By the numbers
          </Typography>
          <Typography variant="h2">Built for scale</Typography>
        </Box>

        <Stack
          direction={{ xs: 'column', md: 'row' }}
          divider={
            <Divider
              orientation="vertical"
              flexItem
              sx={{ borderStyle: 'dashed' }}
            />
          }
          spacing={4}
        >
          {STATS.map((stat) => (
            <Box key={stat.label} sx={{ flex: 1, textAlign: 'center', px: 2 }}>
              <Box sx={{ mb: 2 }}>{stat.icon}</Box>

              <Typography variant="h2" sx={{ mb: 0.5 }}>
                {stat.value}
              </Typography>

              <Typography
                variant="overline"
                sx={{ color: 'text.disabled', display: 'block', mb: 1.5 }}
              >
                {stat.label}
              </Typography>

              <Typography variant="body2" sx={{ color: 'text.secondary', maxWidth: 280, mx: 'auto' }}>
                {stat.description}
              </Typography>
            </Box>
          ))}
        </Stack>
      </Container>
    </Box>
  );
}

// ---------------------------------------------------------------------------
// SSR Verification Section
// ---------------------------------------------------------------------------

function VerificationSection() {
  return (
    <Box
      component="section"
      sx={{
        py: { xs: 8, md: 12 },
        bgcolor: 'primary.darker',
        color: 'common.white',
      }}
    >
      <Container maxWidth="md" sx={{ textAlign: 'center' }}>
        <Typography variant="h3" sx={{ mb: 3 }}>
          Verify SSR is working
        </Typography>

        <Typography sx={{ color: 'rgba(255,255,255,0.72)', mb: 4, fontSize: '1.125rem' }}>
          Right-click anywhere on this page and select &quot;View Page
          Source&quot;. You should see all the content already rendered in the
          HTML, along with{' '}
          <Box
            component="code"
            sx={{
              bgcolor: 'rgba(255,255,255,0.1)',
              px: 1,
              py: 0.25,
              borderRadius: 0.5,
              fontSize: '0.875rem',
            }}
          >
            &lt;style data-emotion&gt;
          </Box>{' '}
          tags in the head containing all MUI styles.
        </Typography>

        <Stack direction="row" spacing={2} justifyContent="center">
          <Button
            variant="contained"
            size="large"
            sx={{
              bgcolor: 'common.white',
              color: 'primary.dark',
              '&:hover': { bgcolor: 'grey.100' },
            }}
            href="/"
          >
            Back to Home
          </Button>
        </Stack>
      </Container>
    </Box>
  );
}

// ---------------------------------------------------------------------------
// About Page
// ---------------------------------------------------------------------------

export function AboutPage() {
  return (
    <>
      <AboutHero />
      <StatsSection />
      <VerificationSection />
    </>
  );
}
