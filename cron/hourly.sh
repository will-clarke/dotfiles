#!/bin/bash

cd ~ && echo `date "+%Y-%m-%d%=%H:%M:%S"` `~/.bin/push_org` >> ~/.cron/cron.log
