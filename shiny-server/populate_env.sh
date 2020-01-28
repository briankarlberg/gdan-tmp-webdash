#!/bin/bash

env > /home/shiny/.Renviron
chown shiny.shiny /home/shiny/.Renviron
/usr/bin/shiny-server.sh
