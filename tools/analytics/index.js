const axios = require('axios')
const _ = require('lodash')
const moment = require('moment')

const POSTHOG_KEY = process.env.WASP_POSTHOG_KEY

ourDistinctIds = [
  'bf3fa7a8-1c11-4f82-9542-ec1a2d28786b',
  '53669068-7441-45eb-b11b-880ad4c9c8c2',
  '380cc449-78db-4bd9-ae29-790e892c63a9',
  '7b9d8578-120c-4c2a-b4a7-3994d2801a24'
]

const main = async () => {
  const events = (await fetchEvents('https://app.posthog.com/api/event/?event=cli'))
        .filter(e => ! ourDistinctIds.includes(e.distinct_id) )

  console.log('\nNumber of CLI events so far:')
  console.log(events.length)

  const eventsByProject = _.groupBy(events, e => e.distinct_id + e.properties.project_hash)

  console.log('\nNumber of Wasp projects created so far:')
  console.log(Object.keys(eventsByProject).length)

  const numProjectsCreatedSinceDatetime = (datetime) => {
    return Object.values(eventsByProject)
      .map(events => moment.min(events.map(e => moment(e.timestamp))))
      .filter(dt => dt.isAfter(datetime))
      .length
  }

  const numProjectsCreatedUntilDatetime = (datetime) => {
    return Object.values(eventsByProject)
      .map(events => moment.min(events.map(e => moment(e.timestamp))))
      .filter(dt => dt.isSameOrBefore(datetime))
      .length
  }

  console.log('\nNumber of Wasp projects that were built so far:')
  console.log(Object.values(eventsByProject).filter(events => events.some(e => e.properties.is_build)).length)

  console.log('\nNumber of Wasp projects created from monday to monday.')
  let monday = moment('2021-01-25')
  while (monday.isBefore(moment.now())) {
    const numProjects = numProjectsCreatedUntilDatetime(monday)
    console.log(numProjects + ', ' + monday.format('YYYY MM DD'))
    monday.add(7, 'days')
  }
}

const fetchEvents = async (url) => {
  console.log('Fetching: ' + url)
  const response = await axios.get(url, { headers: {
    'Authorization': 'Bearer ' + POSTHOG_KEY
  }})
  const { next, results } = response.data
  return results.concat(next ? await fetchEvents(next) : [])
}

main()
