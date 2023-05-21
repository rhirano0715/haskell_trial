#!/bin/bash

self_dir=$(cd $(dirname $0);pwd)

pushd ${self_dir}
    cabal test
    return_code=$?
popd

echo "cabal test end with ${return_code}"

exit ${return_code}
