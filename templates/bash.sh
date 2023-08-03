#!/usr/bin/bash

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

do_something

if [ $? -eq 0 ] ; then
        printf "Yes or no? [Y/N]: %s"
        read -r res
        if [ "$res" == "Y" ];then
            echo yes
        else
            echo no
        fi
fi

exit 0

################
#              #
# End of file  #
#              #
################
