#!/bin/sh

#
# Author: Matthias
#

some_var=0

function usage() {
    echo "Usage: ";
    exit 1
}

while getopts "ha:" o; do
    case "${o}" in
        a)
            som_var=${OPTARG}
            ;;
        h)
            usage
            ;;
    esac
done

exit 0

################
#              #
# End of file  #
#              #
################
