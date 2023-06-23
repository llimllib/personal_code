#!/usr/bin/env bash
docker compose run --build test sh -c 'curl https://reverse_proxy/uuid'
