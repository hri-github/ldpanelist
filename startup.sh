#!/bin/bash

# start process : r shiny app
# remember USE RSCRIPT instead of R -e because latter does not work in sh
# runApp on the dir for two file server files
# redirect nohup output to mounted volume for logs

# note that for running shiny apps directly and not behind a webserver,
#  host = 0.0.0.0 must be specified
nohup Rscript -e "shiny::runApp('root/apps', port=80, , host = '0.0.0.0')" > /root/logs/shiny-app/shinyapp.log 2> shinyapp.err