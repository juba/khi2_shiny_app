#!/bin/sh

rsync -Pavz . humanum:~/zPublish/shiny/khi2/ --exclude .git/
exit 0
