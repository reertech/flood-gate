#!/bin/bash

set -eu
IFS=$'\n\t'

docker build --tag skywriter/flood-gate:latest .
