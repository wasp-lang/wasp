#! /bin/bash

set -e

docker build . -t ask-the-documents-db
docker run --name wasp_db_ask-the-documents -d --rm --publish 5432:5432 -v ask-the-documents:/var/lib/postgresql/data --env POSTGRES_PASSWORD=devpass ask-the-documents-db
