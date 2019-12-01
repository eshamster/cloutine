#!/bin/sh

set -eux

ros use ${LISP}

cd ~/.roswell/local-projects/target
rove *.asd
