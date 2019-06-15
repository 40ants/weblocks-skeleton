#!/bin/bash


if [[ "$1" == "ro" ]]; then
    psql 'host=localhost port=5432 user=lisp_ro password=lisp_ro dbname=lisp'
    exit 0
fi

if [[ "$1" == "empty" ]]; then
    psql 'host=localhost port=5433 user=lisp password=lisp dbname=lisp'
    exit 0
fi

# default
psql 'host=localhost port=5432 user=lisp password=lisp dbname=lisp'

