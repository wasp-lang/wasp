import { RadioGroup } from "@headlessui/react";

export function MyRadio({ options, value, onChange, label }) {
  return (
    <RadioGroup value={value} onChange={onChange} by="value">
      <RadioGroup.Label className="sr-only">{label}</RadioGroup.Label>
      <div className="flex gap-2">
        {options.map((option) => (
          <RadioGroup.Option
            key={option.name}
            value={option}
            disabled={option.disabled}
            className={({ active, checked, disabled }) =>
              `${
                active
                  ? "ring-2 ring-white ring-opacity-60 ring-offset-2 ring-offset-sky-300"
                  : ""
              }
                  ${
                    checked ? "bg-slate-900 bg-opacity-75 text-white" : "bg-white"
                  }
                  ${
                    disabled
                      ? "opacity-50 cursor-not-allowed"
                      : "cursor-pointer"
                  }
                    relative flex cursor-pointer rounded-lg px-5 py-4 shadow-md focus:outline-none flex-1`
            }
          >
            {({ active, checked }) => (
              <>
                <div className="flex w-full items-center justify-between">
                  <div className="flex items-center">
                    <div className="text-sm">
                      <RadioGroup.Label
                        as="p"
                        className={`font-medium  ${
                          checked ? "text-white" : "text-gray-900"
                        }`}
                      >
                        {option.name}
                      </RadioGroup.Label>
                      <RadioGroup.Description
                        as="span"
                        className={`inline ${
                          checked ? "text-slate-100" : "text-gray-500"
                        }`}
                      >
                        {option.description}
                      </RadioGroup.Description>
                    </div>
                  </div>
                  {checked && (
                    <div className="shrink-0 text-white">
                      <CheckIcon className="h-6 w-6" />
                    </div>
                  )}
                </div>
              </>
            )}
          </RadioGroup.Option>
        ))}
      </div>
    </RadioGroup>
  );
}

function CheckIcon(props) {
  return (
    <svg viewBox="0 0 24 24" fill="none" {...props}>
      <circle cx={12} cy={12} r={12} fill="#fff" opacity="0.2" />
      <path
        d="M7 13l3 3 7-7"
        stroke="#fff"
        strokeWidth={1.5}
        strokeLinecap="round"
        strokeLinejoin="round"
      />
    </svg>
  );
}
