#!/bin/bash
shopt -s extglob
./run-yddb-test yddb-tests/!(*.expected)
