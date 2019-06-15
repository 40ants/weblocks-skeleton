#!/bin/bash

set -e

COMMAND=$1

if [[ "$COMMAND" == "dump" ]]; then
    FILENAME="$(date '+%Y%m%d-%H%M%S').sql"
    echo "DUMPING database into ${FILENAME}"
    mkdir -p /db/dumps
    cd /db/dumps
    pg_dump 'host=postgres user=lisp password=lisp' > "$FILENAME"
    rm -fr latest.sql
    ln -s "$FILENAME" latest.sql
    exit 0
fi

if [[ "$COMMAND" == "restore" ]]; then
    FILENAME="${2:-latest.sql}"
    echo "RESTORING database from ${FILENAME}"
    mkdir -p /db/dumps
    cd /db/dumps
    function run_query {
        psql 'host=postgres user=lisp password=lisp dbname=template1' -c "$1"

    }
    run_query 'DROP DATABASE IF EXISTS lisp'
    run_query 'CREATE DATABASE lisp WITH OWNER lisp'
    cat "$FILENAME" | psql 'host=postgres user=lisp password=lisp dbname=lisp'
    exit 0
fi


echo "Give one of supported commands"
echo ""
echo " - dump - Dump dev database to db/dumps/"
echo " - restore [FILENAME] - Restore dev database from a file from db/dumps/, by default - from the latest.sql"
