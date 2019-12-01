#!/bin/sh

set -eux

base_dir=$(dirname ${0})
cd ${base_dir}/..
docker run -e LISP=${LISP} -v $(pwd):/root/.roswell/local-projects/target -t test
