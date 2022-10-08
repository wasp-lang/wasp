import React, { useState } from 'react'

import Container from '@material-ui/core/Container'
import Grid from '@material-ui/core/Grid'
import Tabs from '@material-ui/core/Tabs'
import Tab from '@material-ui/core/Tab'
import Box from '@material-ui/core/Box'
import Typography from '@material-ui/core/Typography'
import Chip from '@material-ui/core/Chip'
import Paper from '@material-ui/core/Paper';
import { makeStyles } from '@material-ui/core/styles'

import useAuth from '@wasp/auth/useAuth.js'
import { useQuery } from '@wasp/queries'

import getTags from '@wasp/queries/getTags'
import getFollowedArticles from '@wasp/queries/getFollowedArticles'
import getAllArticles from '@wasp/queries/getAllArticles'
import Navbar from './Navbar'
import ArticleListPaginated from './article/components/ArticleListPaginated'
import addWaspSourceHeader from './addWaspSourceHeader'

const MainPage = () => {
  const { data: me } = useAuth()

  return (
    <Container maxWidth="lg">
      <Navbar />

      <Grid container spacing={2}>
        <Grid item xs={8}>
          <FeedTabs me={me}/>
        </Grid>
        <Grid item xs={4}>
          <Tags />
        </Grid>
      </Grid>

    </Container>
  )
}

const useStylesFeedTabs = makeStyles((theme) => ({
  root: {
    marginBottom: theme.spacing(2),
  },
}))

const FeedTabs = ({ me }) => {
  const classes = useStylesFeedTabs()

  const [value, setValue] = useState(1);
  
  const handleChange = (event, newValue) => setValue(newValue)

  return (
    <>
      <Tabs value={value} onChange={handleChange} className={classes.root}>
        <Tab label="Your Feed" id="feed-tabpanel-0"/>
        <Tab label="Global Feed" id="feed-tabpanel-1"/>
      </Tabs>

      <TabPanel value={value} index={0}>
        { me && (
          <ArticleListPaginated
            query={getFollowedArticles}
            makeQueryArgs={({ skip, take }) => ({ skip, take })}
            pageSize={5}
          />
        )}
      </TabPanel>

      <TabPanel value={value} index={1}>
        <ArticleListPaginated
          query={getAllArticles}
          makeQueryArgs={({ skip, take }) => ({ skip, take })}
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

const useStylesTags = makeStyles((theme) => ({
  root: {
    padding: theme.spacing(0.5),
    margin: 0,
  },
  chip: {
    margin: theme.spacing(0.5),
  },
  title: {
    marginLeft: theme.spacing(0.5)
  }
}))

const Tags = () => {
  const classes = useStylesTags()

  const { data: tags } = useQuery(getTags)

  if (!tags) return null

  const popularTags = tags.sort((a, b) => parseInt(b.numArticles, 10) - parseInt(a.numArticles, 10)).slice(0, 10);

  return (
    <Paper className={classes.root}>
      <Typography variant="subtitle1" className={classes.title}>Popular tags</Typography>
        { popularTags.map(tag => (
          <Chip
            className={classes.chip}
            label={`${tag.name} (${tag.numArticles})`}
          />
        ))}
    </Paper>
  )
}

export default addWaspSourceHeader(MainPage)
