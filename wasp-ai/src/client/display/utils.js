import { availableColors } from "../components/Color";

export function getColorValue(colorName) {
  return availableColors.find((color) => color.name === colorName).color;
}

export function getStatusName(status) {
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

export function getStatusText(status) {
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
