#!/bin/bash

function main  {
    cabal test
    local rc=${?}
    echo "test end with ${rc}"
    return ${rc}
}

main
exit ${?}

