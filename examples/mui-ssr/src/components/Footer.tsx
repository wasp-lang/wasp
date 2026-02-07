import Box from '@mui/material/Box';
import Container from '@mui/material/Container';
import Divider from '@mui/material/Divider';
import Typography from '@mui/material/Typography';
import Stack from '@mui/material/Stack';
import Link from '@mui/material/Link';

export function Footer() {
  return (
    <Box component="footer" sx={{ bgcolor: 'grey.900', color: 'common.white' }}>
      <Container maxWidth="lg" sx={{ py: 8 }}>
        <Stack
          direction={{ xs: 'column', md: 'row' }}
          spacing={4}
          justifyContent="space-between"
        >
          <Box>
            <Typography variant="h5" sx={{ fontWeight: 800, mb: 1 }}>
              Wasp MUI
            </Typography>
            <Typography variant="body2" sx={{ color: 'grey.500', maxWidth: 320 }}>
              A minimal example demonstrating MUI with server-side rendering
              in the Wasp framework, featuring Emotion CSS-in-JS extraction.
            </Typography>
          </Box>

          <Stack direction="row" spacing={5}>
            <Stack spacing={1.5}>
              <Typography variant="overline" sx={{ color: 'grey.500' }}>
                Pages
              </Typography>
              <Link href="/" color="inherit" underline="hover" variant="body2">
                Home
              </Link>
              <Link href="/about" color="inherit" underline="hover" variant="body2">
                About
              </Link>
            </Stack>

            <Stack spacing={1.5}>
              <Typography variant="overline" sx={{ color: 'grey.500' }}>
                Resources
              </Typography>
              <Link
                href="https://wasp-lang.dev/docs"
                target="_blank"
                rel="noopener"
                color="inherit"
                underline="hover"
                variant="body2"
              >
                Wasp Docs
              </Link>
              <Link
                href="https://mui.com"
                target="_blank"
                rel="noopener"
                color="inherit"
                underline="hover"
                variant="body2"
              >
                MUI
              </Link>
            </Stack>
          </Stack>
        </Stack>

        <Divider sx={{ my: 4, borderColor: 'grey.800' }} />

        <Typography variant="body2" sx={{ color: 'grey.600', textAlign: 'center' }}>
          Built with Wasp + MUI + Emotion SSR
        </Typography>
      </Container>
    </Box>
  );
}
