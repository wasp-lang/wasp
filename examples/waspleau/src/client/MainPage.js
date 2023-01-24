import React from 'react'

import refreshDashboardData from '@wasp/queries/dashboard'
import { useQuery } from '@wasp/queries'

import './style.css'
import addWaspSourceHeader from './addWaspSourceHeader'

const MainPage = () => {
  const { data: dashboardData, isFetching, error } = useQuery(refreshDashboardData, null, { refetchInterval: 60 * 1000 })

  return (
    <div style={{ minWidth: "800px" }}>

      <div className="container">
        <div className="dashboard">
          {dashboardData && <DashboardData items={dashboardData} />}
          {isFetching && 'Fetching...'}
          {error && 'Error: ' + error}
        </div>
      </div>
    </div>
  )
}

const DashboardItem = (props) => (
  <div className="dashboard-item">
    <p className="title">{props.item.name}</p>
    <p className="value">{props.item.value || "-"}</p>
    <p className="footer">{props.item.updatedAt && ("Updated At: " + (new Date(props.item.updatedAt)).toLocaleString())}</p>
  </div>
)

const DashboardData = (props) => {
  if (!props.items?.length) return 'No dashboard items'
  return props.items.map((item) => <DashboardItem item={item} key={item.name} />)
}

export default addWaspSourceHeader(MainPage)
