#!/bin/sh
#sudo fuser -k -TERM -n tcp 3000
#echo "zabijanie procesu tcp:3000"
stack build --copy-bins 
cd /home/kryn/.local/bin
./agda-gpt-assistant-exe x1 x2 x3 4 /home/kryn/agda-gpt-assistant/agda-gpt-assistant/data/config/config1.json

