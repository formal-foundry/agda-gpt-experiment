#!/bin/sh
#sudo fuser -k -TERM -n tcp 3000
#echo "zabijanie procesu tcp:3000"
stack build --copy-bins 
cd /home/kryn/.local/bin
./agda-gpt-assistant-exe T3.agda "length : {A : Set} → List A → Nat" x3 4 /home/kryn/tt/agda-gpt-assistant/agda-gpt-assistant/data/config/config1.json


# -- bin [agda_file_name] [task description] [operation mode] [max turns] [config name]  

