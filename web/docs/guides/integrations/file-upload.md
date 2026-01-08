---
last_update:
  date: 2024-03-14
title: File Uploads
comments: true
---

# File Uploads

This guide shows you how to implement file uploads in your Wasp application using [Multer](https://github.com/expressjs/multer).

## Prerequisites

Make sure you have a Wasp project set up. If you haven't, follow the [Getting Started](../../introduction/quick-start.md) guide first.

## Setting up File Uploads

### 1. Install Multer

Install the Multer package and its types:

```bash
npm install multer
npm install --save-dev @types/multer
```

### 2. Define the API endpoint in main.wasp

Create an API namespace with middleware configuration and the upload endpoint:

```wasp title="main.wasp"
app FileUpload {
  wasp: {
    version: "^0.15.0"
  },
  title: "file-upload",
}

route RootRoute { path: "/", to: MainPage }
page MainPage {
  component: import { MainPage } from "@src/MainPage.jsx"
}

apiNamespace fileUploadMiddleware {
  middlewareConfigFn: import { addMiddleware } from "@src/apis.js",
  path: "/api/upload"
}

api fileUpload {
  httpRoute: (POST, "/api/upload"),
  fn: import { uploadFile } from "@src/apis.js",
  entities: []
}
```

### 3. Create the API handlers

Create the middleware configuration and upload handler:

```ts title="src/apis.ts"
import { MiddlewareConfigFn } from "wasp/server";
import { FileUpload } from "wasp/server/api";
import multer from "multer";

const upload = multer({ dest: "uploads/" });

export const addMiddleware: MiddlewareConfigFn = (config) => {
  config.set("multer", upload.single("file"));
  return config;
};

export const uploadFile: FileUpload = (req, res) => {
  console.log(req.body);
  console.log(req.file);
  const file = req.file!;
  return res.json({
    fileExists: !!file,
  });
};
```

### 4. Create the upload form

Create a form component to handle file uploads:

```jsx title="src/MainPage.jsx"
import { useState } from "react";
import "./Main.css";
import { api } from "wasp/client/api";

export const MainPage = () => {
  const [name, setName] = useState("");
  const [myFile, setMyFile] = useState();

  const handleSubmit = async (e) => {
    if (!myFile) {
      alert("Please select a file");
      return;
    }
    e.preventDefault();
    const formData = new FormData();
    formData.append("name", name);
    formData.append("file", myFile);
    try {
      const data = await api.post("/api/upload", formData);
      alert(JSON.stringify(data.data, null, 2));
    } catch (err) {
      console.log(err);
    }
  };

  return (
    <div className="container">
      <main>
        <h1>Uploading files with Wasp</h1>
        <form onSubmit={handleSubmit}>
          <div className="form-group">
            <label htmlFor="name">Name</label>
            <input
              type="text"
              id="name"
              name="name"
              value={name}
              onChange={(e) => setName(e.target.value)}
            />
          </div>
          <div className="form-group">
            <label htmlFor="my-file">My File</label>
            <input
              type="file"
              id="my-file"
              name="my-file"
              onChange={(e) => setMyFile(e.target.files[0])}
            />
          </div>
          {myFile && (
            <div className="form-group">
              <strong>Selected file:</strong> {myFile.name}
            </div>
          )}
          <button type="submit">Upload the file</button>
        </form>
      </main>
    </div>
  );
};
```

## Customizing Upload Settings

### Change upload destination

You can customize where files are stored:

```ts
const upload = multer({ dest: "my-custom-uploads/" });
```

### Limit file size

Add file size limits:

```ts
const upload = multer({
  dest: "uploads/",
  limits: {
    fileSize: 5 * 1024 * 1024, // 5MB limit
  },
});
```

### Filter file types

Only accept certain file types:

```ts
const upload = multer({
  dest: "uploads/",
  fileFilter: (req, file, cb) => {
    if (file.mimetype.startsWith("image/")) {
      cb(null, true);
    } else {
      cb(new Error("Only images are allowed"));
    }
  },
});
```

### Handle multiple files

To handle multiple file uploads:

```ts
export const addMiddleware: MiddlewareConfigFn = (config) => {
  config.set("multer", upload.array("files", 10)); // Max 10 files
  return config;
};
```

For more options, see the [Multer documentation](https://github.com/expressjs/multer).
