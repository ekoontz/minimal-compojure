#!/bin/sh
# need this until i figure out how to get ring
# to reload all the files. some files 
# (e.g. src/italianverbs/core.clj) seem
# to trigger a reload, while others don't. 
# (e.g. src/italianverbvs/morphology.clj does not).
while [ 1 ]; do
    echo "touching.."
    touch src/italianverbs/core.clj
    sleep 1

done