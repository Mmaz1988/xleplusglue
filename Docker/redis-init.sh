#!/bin/sh
set -eu

redis-server --appendonly yes --dir /data --save 60 1 --bind 0.0.0.0 &
redis_pid="$!"
uvicorn_pid=""

cleanup() {
  if [ -n "$uvicorn_pid" ]; then
    kill "$uvicorn_pid" >/dev/null 2>&1 || true
  fi
  kill "$redis_pid" >/dev/null 2>&1 || true
}

trap cleanup INT TERM EXIT

until redis-cli ping >/dev/null 2>&1; do
  sleep 0.1
done

redis-cli SET last_session '{"modules":{},"updated_at":null,"data":{}}' NX >/dev/null

uvicorn Redis.redis_endpoints:app --host 0.0.0.0 --port 8083 &
uvicorn_pid="$!"

wait "$uvicorn_pid"
