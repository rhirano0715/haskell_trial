#!/bin/bash

echo $0

SELF_DIR=$(cd $(dirname $0);pwd)

function main  {
    local dir=$1
    local dir_bk=$(pwd)

    cd ${dir}
    cabal test
    local rc=${?}
    cd ${dir_bk}

    echo "test end with ${rc}"
    return ${rc}
}

main "${SELF_DIR}"
exit ${?}

