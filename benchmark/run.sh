#!/bin/bash

# This script runs the benchmarking suite with the given main file.
# If no main is given the full suite is run.

set -e
set -E

if [[ -z "$1" ]]; then
	MAIN=""
else
	MAIN="-m $1"
fi

COMMIT=`git rev-parse HEAD | cut -c1-8`
DATE=`date "+%y%m%d"`
REVISION=`grep defproject project.clj | cut -d ' ' -f 3 | tr -d '"'`
OUTPUT="benchmark/results/$REVISION-$DATE-$COMMIT.txt"

echo "Opening file $OUTPUT"

{
	echo "Version: $REVISION"
	echo "Date:" `date`
	echo "Commit: $COMMIT"
	echo "Main: $MAIN"

	echo "lein with-profile dev,benchmark run $MAIN"
	lein with-profile dev,benchmark run $MAIN
} | tee "$OUTPUT"
