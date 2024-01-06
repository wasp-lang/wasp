import { validProjectBrandColors } from "../components/Color";

export function getTailwindClassNameForProjectBrandColor(colorName) {
  return validProjectBrandColors.find((color) => color.name === colorName).color;
}

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

export function projectStatusToDisplayableText(status) {
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
