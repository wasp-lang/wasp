import { existsSync } from "fs";
import path from "path";
import { fileURLToPath } from "url";

export const BANNER_PATH = "/banner-images";

export const DEFAULT_BANNER_IMAGE = "default-banner.webp";

export const getBannerImageFilename = ({ path }: { path: string }) =>
  path.replace(/.*\//, "").replace(/\.\w+$/, ".webp");

export const checkBannerImageExists = ({
  bannerImageFileName,
}: {
  bannerImageFileName: string;
}) => {
  const __dirname = path.dirname(fileURLToPath(import.meta.url));
  const imagePath = path.join(
    __dirname,
    `../../public/${BANNER_PATH}`,
    bannerImageFileName,
  );
  return existsSync(imagePath);
};
