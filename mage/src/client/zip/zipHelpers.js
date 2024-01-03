import * as zip from "@zip.js/zip.js";

// Input: object with file paths as keys and file contents as values
export function createFilesAndDownloadZip(files, zipName) {
  const zipWriter = new zip.ZipWriter(new zip.BlobWriter("application/zip"), {
    bufferedWrite: true,
    useCompressionStream: false,
  });

  // Create a file in the zip for each file in the input
  for (let [path, contents] of Object.entries(files)) {
    zipWriter.add(path, new zip.TextReader(contents));
  }

  // Close the zip and get a blob containing the zip contents
  zipWriter.close().then((blob) => {
    // Download the zip
    const zipUrl = URL.createObjectURL(blob);
    const link = document.createElement("a");
    link.href = zipUrl;
    link.download = `${zipName}.zip`;
    link.click();
  });
}
