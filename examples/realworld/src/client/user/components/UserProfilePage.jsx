import React, { useState } from 'react'
import { Link, useHistory } from 'react-router-dom'

import Container from '@material-ui/core/Container'
import Grid from '@material-ui/core/Grid'
import Tabs from '@material-ui/core/Tabs'
import Tab from '@material-ui/core/Tab'
import Box from '@material-ui/core/Box'
import Button from '@material-ui/core/Button'
import { makeStyles } from '@material-ui/core/styles'

import useAuth from '@wasp/auth/useAuth'
import { useQuery } from '@wasp/queries'

import getUser from '@wasp/queries/getUser'
import getArticlesByUser from '@wasp/queries/getArticlesByUser'
import getFavoritedArticles from '@wasp/queries/getFavoritedArticles'
import followUser from '@wasp/actions/followUser'
import Navbar from '../../Navbar'
import addWaspSourceHeader from '../../addWaspSourceHeader'
import ArticleListPaginated from '../../article/components/ArticleListPaginated'
import smileyImageUrl from '../../smiley.jpg'

const useStyles = makeStyles((theme) => ({
  articles: {
    marginTop: theme.spacing(5)
  }
}))


const UserProfilePage = (props) => {
  const history = useHistory()

  const { data: me } = useAuth()

  const username = props.match.params.username
  const { data: user, error: userError } = useQuery(getUser, { username })

  // NOTE: use-query will retry multiple times in case of error, making user
  //   wait relatively long before giving up on for example 404 and returning error,
  //   while on the other hand we would probably like it to give up on 404 immediatelly!
  //   Should we modify react-query default settings to not do any retrying?
  if (userError) {
    console.log(userError)
    history.push("/")
  }

  return user ? (
    <Container maxWidth="lg">
      <Navbar />

      <Grid container direction="row" justify="center">
        <Grid item xs={8}>
          <img src={user.profilePictureUrl || smileyImageUrl} alt="Profile" />
          <p> { user.username } </p>
          <p> { user.bio } </p>
          { me && me.username === username && (
            <div>
              <Button component={ Link } to="/settings" variant="contained" color="primary">
                Edit Profile Settings
              </Button>
            </div>
          )}
          { me && me.username !== username && (
            <div>
              <FollowUserButton user={user} />
            </div>
          )}

          <Articles user={user} />
        </Grid>
      </Grid>

    </Container>
  ) : null
}

const FollowUserButton = (props) => {
  const user = props.user
  const { data: me } = useAuth()

  const toggleFollow = async () => {
    try {
      followUser({ username: user.username, follow: !user.following })
    } catch (err) {
      console.error(err)
      window.alert(err)
    }
  }

  return me && me.username !== user.username ? (
    <button onClick={toggleFollow}>
      { user.following ? 'Unfollow' : 'Follow' }
    </button>
  ) : null
}

const useStylesFeedTabs = makeStyles((theme) => ({
  root: {
    marginBottom: theme.spacing(2),
  },
}))

const ProfileFeedTabs = (props) => {
  const classes = useStylesFeedTabs()

  const [value, setValue] = useState(0);
  
  const handleChange = (event, newValue) => setValue(newValue)

  return (
    <>
      <Tabs value={value} onChange={handleChange} className={classes.root}>
        <Tab label="My Articles" id="feed-tabpanel-0"/>
        <Tab label="Favorited articles" id="feed-tabpanel-1"/>
      </Tabs>

      <TabPanel value={value} index={0}>
        <ArticleListPaginated
          query={getArticlesByUser}
          makeQueryArgs={({ skip, take }) => ({ username: props.user.username, skip, take })}
          pageSize={5}
        />
      </TabPanel>

      <TabPanel value={value} index={1}>
        <ArticleListPaginated
          query={getFavoritedArticles}
          makeQueryArgs={({ skip, take }) => ({ username: props.user.username, skip, take })}
          pageSize={5}
        />
      </TabPanel>
    </>
  )
}

function TabPanel(props) {
  const { children, value, index, ...other } = props

  return (
    <div
      role="tabpanel"
      hidden={value !== index}
      id={`feed-tabpanel-${index}`}
      {...other}
    >
      {value === index && (
        <Box>
          {children}
        </Box>
      )}
    </div>
  )
}

const Articles = (props) => {
  const classes = useStyles()

  return (
    <div className={classes.articles}>
      <ProfileFeedTabs {...props} />
    </div>
  )
}

export default addWaspSourceHeader(UserProfilePage)
