#!/usr/bin/bash

#
# Author: Matthias
#

ps axf | grep -v grep | grep -q '/usr/local/bin/emacs'

echo ${?}

if [[ $? -eq 1 ]] ;
then
    emacs &
else
    EMACSCLIENT=/usr/local/bin/emacsclient
    ${EMACSCLIENT} -e '(make-frame)'
fi

################
#              #
# End of file  #
#              #
################
