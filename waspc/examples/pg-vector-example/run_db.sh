#! /bin/bash

set -e

docker run --name wasp_db_pg-vector-example -d --rm --publish 5432:5432 -v pg-vector-example:/var/lib/postgresql/data --env POSTGRES_PASSWORD=devpass ankane/pgvector