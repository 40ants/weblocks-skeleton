#!/bin/bash

set -e
set -x

/app/server \
    | tee -a /app/logs/app.log \
    | jsail
