#!/usr/bin/python3

import logging
import logging.handlers
import signal
import sys
import os
import time
import argparse

# Author     : Matthias
# Description: Python script template

class Application:

    name               = ''
    version            = ''
    log                = None
    parser             = None
    args               = None
    flevel             = 0

    def __init__(self):
        signal.signal(signal.SIGINT, Application.signal_int_handler)
        parser = argparse.ArgumentParser(description="", epilog="")
        parser.add_argument("-v", "--verbose", help="Be more verbose when logging", action="store_true")
        parser.add_argument("-l", "--logdomain", help="Domain for logging", default="this-script")
        parser.add_argument("-d", "--directory", help"Root directory for the traversal")
        self.args = parser.parse_args()
        self.parser = parser
        self.setup_logging()

    def setup_logging(self):
        """ Setup logging so that a root logger is configured with formatter and handler
        according to configuration. Additional loggers should just propagate to the root
        logger. """
        self.log = logging.getLogger(self.args.logdomain)
        rootlogger = logging.getLogger()
        formatstring='%(asctime)s %(levelname)-15s %(name)s # %(message)s'
        formatter = logging.Formatter(fmt=formatstring, datefmt='%d.%m.%y %I:%M:%S')
        handler = logging.StreamHandler(sys.stderr)
        handler.setFormatter(formatter)
        rootlogger.addHandler(handler)
        level = logging.INFO
        if self.args.verbose:
            level = logging.DEBUG
        self.log.setLevel(level)
        rootlogger.setLevel(level)
        self.log.propagate=1

    @staticmethod
    def signal_int_handler(signal, frame):
        interrupt_msg = '\r\n\r\n{} {} terminated by keyboard interrupt'.format(Application.name, Application.version)
        print(interrupt_msg)
        exit(0)

    def process_file(self, fname):
        i=0
        while i < flevel:
            print("  ")
            i+=1
        print(f"{fname}")

    def run(self):
        # Set the directory you want to start from
        rootDir = self.args.directory
        for dirName, subdirList, fileList in os.walk(rootDir):
            print( f"Found directory: {dirName}" )
            for fname in fileList:
                self.process_file(fname)

def main():
    app = Application()
    app.log.info('{} {} is starting'.format(app.name, app.version))
    app.run()
    app.log.info('{} {} is done'.format(app.name, app.version))

if __name__ == '__main__':
    main()

#
# Done
#
# # # end of script