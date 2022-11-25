import React from 'react'
import { Link } from 'react-router-dom'

import AppBar from '@material-ui/core/AppBar'
import Toolbar from '@material-ui/core/Toolbar'
import Button from '@material-ui/core/Button'
import Typography from '@material-ui/core/Typography'
import { makeStyles } from '@material-ui/core/styles'

import useAuth from '@wasp/auth/useAuth.js'


const useStyles = makeStyles((theme) => ({
  root: {
    flexGrow: 1,
    marginBottom: 50
  },
  title: {
    flexGrow: 1,
  },
}));

const Navbar = () => {
  const classes = useStyles()

  const { data: user } = useAuth()
  if (user) {
    return (
      <div className={classes.root}>
        <AppBar position="static">
          <Toolbar>
            <Typography variant="h6" className={classes.title}>Conduit</Typography>

            <Button component={ Link } to="/" color="inherit">Home</Button>
            <Button component={ Link } to="/editor" color="inherit">New Article</Button>
            <Button component={ Link } to="/settings" color="inherit">Settings</Button>
            <Button component={ Link } to={`/@${user.username}`} color="inherit">{ user.username }</Button>

          </Toolbar>
        </AppBar>
      </div>
    )
  } else {
    return (
      <div className={classes.root}>
        <AppBar position="static">
          <Toolbar>
            <Typography variant="h6" className={classes.title}>Conduit</Typography>

            <Button component={ Link } to="/login" color="inherit">Sign in</Button>
            <Button component={ Link } to="/register" color="inherit">Sign up</Button>

          </Toolbar>
        </AppBar>
      </div>
    )
  }
}

export default Navbar
