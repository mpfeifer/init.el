#!/usr/bin/sh

#
# Author: Matthias
#

EMACSCLIENT=/usr/local/bin/emacsclient
ZENITY=/usr/bin/zenity

${EMACSCLIENT} -c -e '(mpx-org-capture-full-frame)'

if [[ $? == 1 ]] ;
then
    ${ZENITY} --info --text="Looks like Emacs is not running"
fi

################
#              #
# End of file  #
#              #
################
