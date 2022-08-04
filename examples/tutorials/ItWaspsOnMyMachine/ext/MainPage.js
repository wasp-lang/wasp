import React, { useState } from 'react'
import { useQuery } from '@wasp/queries'
import getExcuse from '@wasp/queries/getExcuse'
import getAllExcuses from '@wasp/queries/getAllExcuses'
import saveExcuse from '@wasp/actions/saveExcuse'
import './Main.css'

const MainPage = () => {
  const [currentExcuse, setCurrentExcuse] = useState({ text: "" })
  const { data: excuses } = useQuery(getAllExcuses)

  const handleGetExcuse = async () => {
    try {
      setCurrentExcuse(await getExcuse())
    } catch (err) {
      window.alert('Error while deleting list: ' + err.message)
    }
  }

  const handleSaveExcuse = async () => {
    try {
      if (currentExcuse.text) {
        await saveExcuse(currentExcuse)
      }
    } catch (err) {
      window.alert('Error while deleting list: ' + err.message)
    }
  }

  return (
    <div className="grid grid-cols-2 text-3xl">
      <div>
        <div className="bg-white space-x-2 rounded-lg border border-gray-200 text-gray-900">
          <div className="px-6 py-2 border-b border-gray-200 w-full rounded-t-lg bg-blue-600 text-white"> Current excuse: </div>
          <Excuse excuse={currentExcuse} />
          <button onClick={handleGetExcuse} className="items-center bg-blue-600 text-white hover:bg-blue-400 p-2 rounded w-auto"> Get excuse </button>
          <button onClick={handleSaveExcuse} className="items-center bg-blue-600 text-white hover:bg-blue-400 p-2 rounded w-auto"> Save excuse </button>
        </div>
      </div>
      <div>
        <div className="bg-white rounded-lg border border-gray-200 text-gray-900">
          <div className="px-6 py-2 border-b border-gray-200 w-full rounded-t-lg bg-blue-600 text-white"> Saved excuses: </div>
          {excuses && <ExcuseList excuses={excuses} />}
        </div>
      </div>
    </div>
  )
}

const ExcuseList = (props) => {
  if (!props.excuses?.length) return 'No saved excuses'
  return props.excuses.map((excuse, idx) => <Excuse excuse={excuse} key={idx} />)
}

const Excuse = ({ excuse }) => {
  return (
    <div className="px-6 py-2 border-b border-gray-200 w-full">
      {excuse.text}
    </div>
  )
}

export default MainPage
