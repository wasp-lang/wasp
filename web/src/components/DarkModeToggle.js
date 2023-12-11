import React, { useState } from 'react'
import { Sun, Moon } from 'react-feather'

const DarkModeToggle = () => {
  const [isDarkMode, setIsDarkMode] = useState(false)

  const toggleDarkMode = () => {
    setIsDarkMode(!isDarkMode)
  }

  return (
    <div className="flex items-end">
      <Sun strokeWidth={2} size={22} className="text-neutral-500" />
      <button
        type="button"
        aria-pressed="false"
        className={`
          relative mx-3
          inline-flex h-6 w-11 flex-shrink-0 cursor-pointer
          rounded-full border-2 border-transparent
          bg-neutral-500
          transition-colors duration-200 ease-in-out focus:outline-none
        `}
        onClick={() => toggleDarkMode()}
      >
        <span
          aria-hidden="true"
          className={`
            ${isDarkMode ? 'translate-x-5' : 'translate-x-0'}
            inline-block h-5 w-5
            transform rounded-full bg-white shadow-lg
            ring-0 transition duration-200 ease-in-out
          `}
        />
      </button>

      <Moon strokeWidth={2} size={22} className="text-neutral-500" />
    </div>
  )
}

export default DarkModeToggle
