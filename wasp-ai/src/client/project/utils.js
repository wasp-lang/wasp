import { validProjectBrandColors } from "../components/Color";

// converts project's color name to tailwind css class name
export function getTailwindClassNameForProjectBrandColor(colorName) {
  return validProjectBrandColors.find((color) => color.name === colorName).color;
}

// converts project's status to tailwind css class name
export function getTailwindClassNameForProjectStatus(status) {
  switch (status) {
    case "in-progress":
      return "inProgress";
    case "success":
      return "success";
    case "failure":
      return "error";
    case "cancelled":
      return "cancelled";
    default:
      return "idle";
  }
}

// converts project's status to displayable text
export function getDisplayableTextForProjectStatus(status) {
  switch (status) {
    case "in-progress":
      return "In progress";
    case "success":
      return "Success";
    case "failure":
      return "Error";
    case "cancelled":
      return "Cancelled";
    case "pending":
      return "Pending";
    default:
      return "Unknown";
  }
}
