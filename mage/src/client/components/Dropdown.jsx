import { Fragment } from "react";
import { Listbox, Transition } from "@headlessui/react";
import { CheckIcon, ChevronUpDownIcon } from "@heroicons/react/20/solid";
import { Color } from "./Color";

export function MyDropdown({ value, onChange, options }) {
  return (
    <Listbox value={value} onChange={onChange}>
      <div className="relative mt-1">
        <Listbox.Button className="relative w-full rounded-lg bg-white py-3 pl-3 pr-10 text-left shadow-md focus:outline-none focus-visible:border-indigo-500 focus-visible:ring-2 focus-visible:ring-white focus-visible:ring-opacity-75 focus-visible:ring-offset-2 focus-visible:ring-offset-sky-300 cursor-pointer">
          <Option value={value} />
          <span className="pointer-events-none absolute inset-y-0 right-0 flex items-center pr-2">
            <ChevronUpDownIcon
              className="h-5 w-5 text-gray-400"
              aria-hidden="true"
            />
          </span>
        </Listbox.Button>
        <Transition
          as={Fragment}
          leave="transition ease-in duration-100"
          leaveFrom="opacity-100"
          leaveTo="opacity-0"
        >
          <Listbox.Options className="absolute mt-1 max-h-60 w-full overflow-auto rounded-md bg-white py-1 text-base shadow-lg ring-1 ring-black ring-opacity-5 focus:outline-none sm:text-sm z-10">
            {options.map((option, optionIdx) => (
              <Listbox.Option
                key={optionIdx}
                className={({ active }) =>
                  `relative cursor-default select-none py-2 pl-10 pr-4 text-gray-900
                   ${option.disabled ? "text-slate-400" : "cursor-pointer"}
                   ${active ? "bg-slate-100 text-slate-900" : null} 
                  `
                }
                value={option}
                disabled={option.disabled}
              >
                {({ selected }) => (
                  <>
                    <Option value={option} selected={selected} showDescription={true} />
                    {selected ? (
                      <span className="absolute inset-y-0 left-0 flex items-center pl-3 text-slate-600">
                        <CheckIcon className="h-5 w-5" aria-hidden="true" />
                      </span>
                    ) : null}
                  </>
                )}
              </Listbox.Option>
            ))}
          </Listbox.Options>
        </Transition>
      </div>
    </Listbox>
  );
}

function Option({ value, selected = false, showDescription = false }) {
  return (
    <span
      className={`flex flex-col ${
        selected ? "font-medium" : "font-normal"
      }`}
    >
      <span className="flex flex-row truncate">
        {value.color && (
          <span className="mr-2">
            <Color value={value.color} />
          </span>
        )}
        {value.name}
      </span>
      { showDescription && value.description && (
        <span className="text-slate-800 font-light text-xs">
          {value.description}
        </span>
      )}
    </span>
  );
}
