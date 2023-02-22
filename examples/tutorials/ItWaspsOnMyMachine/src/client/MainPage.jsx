import React, { useState } from 'react'
import { useQuery } from '@wasp/queries'
import getExcuse from '@wasp/queries/getExcuse'
import getAllSavedExcuses from '@wasp/queries/getAllSavedExcuses'
import saveExcuse from '@wasp/actions/saveExcuse'

const MainPage = () => {
  const [currentExcuse, setCurrentExcuse] = useState({ text: "" })
  const { data: excuses } = useQuery(getAllSavedExcuses)

  const handleGetExcuse = async () => {
    try {
      setCurrentExcuse(await getExcuse())
    } catch (err) {
      window.alert('Error while getting the excuse: ' + err.message)
    }
  }

  const handleSaveExcuse = async () => {
    if (currentExcuse.text) {
      try {
        await saveExcuse(currentExcuse)
      } catch (err) {
        window.alert('Error while saving the excuse: ' + err.message)
      }
    }
  }

  return (
    <div className="grid grid-cols-2 text-3xl">
      <div>
          <button onClick={handleGetExcuse} className="mx-2 my-1 p-2 bg-blue-600 hover:bg-blue-400 text-white rounded"> Get excuse </button>
          <button onClick={handleSaveExcuse} className="mx-2 my-1 p-2 bg-blue-600 hover:bg-blue-400 text-white rounded"> Save excuse </button>
        <Excuse excuse={currentExcuse} />
      </div>
      <div>
        <div className="px-6 py-2 bg-blue-600 text-white"> Saved excuses: </div>
        {excuses && <ExcuseList excuses={excuses} />}
      </div>
    </div>
  )
}

const ExcuseList = (props) => {
  return props.excuses?.length ?  props.excuses.map((excuse, idx) => <Excuse excuse={excuse} key={idx} />) : 'No saved excuses'
}

const Excuse = ({ excuse }) => {
  return (
    <div className="px-6 py-2">
      {excuse.text}
    </div>
  )
}

export default MainPage
