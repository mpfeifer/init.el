#!/usr/bin/bash

#
# Author: Matthias
#

EMACSCLIENT=/usr/local/bin/emacsclient

${EMACSCLIENT} --no-wait \
               --create-frame \
               --alternate-editor=/usr/local/bin/emacs \
               $1

################
#              #
# End of file  #
#              #
################
