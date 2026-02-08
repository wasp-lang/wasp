import AppBar from '@mui/material/AppBar';
import Box from '@mui/material/Box';
import Button from '@mui/material/Button';
import Container from '@mui/material/Container';
import Toolbar from '@mui/material/Toolbar';
import Typography from '@mui/material/Typography';
import { Link as RouterLink } from 'react-router';

const NAV_ITEMS = [
  { label: 'Home', path: '/' },
  { label: 'About', path: '/about' },
];

export function Header() {
  return (
    <AppBar
      position="sticky"
      sx={{
        bgcolor: 'rgba(255, 255, 255, 0.9)',
        backdropFilter: 'blur(8px)',
        borderBottom: 1,
        borderColor: 'divider',
      }}
    >
      <Container maxWidth="lg">
        <Toolbar disableGutters sx={{ height: 72 }}>
          <Typography
            component={RouterLink}
            to="/"
            variant="h5"
            sx={{
              color: 'primary.main',
              textDecoration: 'none',
              fontWeight: 800,
              mr: 4,
            }}
          >
            Wasp MUI
          </Typography>

          <Box sx={{ display: 'flex', gap: 1 }}>
            {NAV_ITEMS.map((item) => (
              <Button
                key={item.path}
                component={RouterLink}
                to={item.path}
                sx={{
                  color: 'text.primary',
                  fontWeight: 600,
                  '&:hover': {
                    color: 'primary.main',
                    bgcolor: 'transparent',
                  },
                }}
              >
                {item.label}
              </Button>
            ))}
          </Box>

          <Box sx={{ flexGrow: 1 }} />

          <Button
            component={RouterLink}
            to="/about"
            variant="contained"
            color="primary"
            size="large"
          >
            Get Started
          </Button>
        </Toolbar>
      </Container>
    </AppBar>
  );
}
