#!/bin/bash

for ((i=1; i<=1000; i++))
do
    username="mihodeputa$i"
    password="12345678"
    address="Moja adresa 12"

    echo "Creating user $username"

    curl 'https://miho-migration-test-server.fly.dev/auth/local/signup' \
        -H 'authority: miho-migration-test-server.fly.dev' \
        -H 'accept: application/json, text/plain, */*' \
        -H 'accept-language: en-US,en;q=0.9' \
        -H 'content-type: application/json' \
        -H 'dnt: 1' \
        -H 'origin: https://miho-migration-test-client.fly.dev' \
        -H 'referer: https://miho-migration-test-client.fly.dev/' \
        -H 'sec-ch-ua: "Not(A:Brand";v="24", "Chromium";v="122"' \
        -H 'sec-ch-ua-mobile: ?0' \
        -H 'sec-ch-ua-platform: "macOS"' \
        -H 'sec-fetch-dest: empty' \
        -H 'sec-fetch-mode: cors' \
        -H 'sec-fetch-site: cross-site' \
        -H 'user-agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36' \
        --data-raw "{\"username\":\"$username\",\"password\":\"$password\",\"address\":\"$address\"}"
done
