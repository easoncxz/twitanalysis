#!/usr/bin/env bash

# https://blog.jez.io/ruby-virtualenvs/
export GEM_HOME=.gems
export GEM_PATH=

# For a .env file oneline-interpreter in Bash:
# https://gist.github.com/judy2k/7656bfe3b322d669ef75364a46327836

# For a better `activate` scripts: (from 2012)
# https://datagrok.org/python/activate/
export VIRTUAL_GEM_ENV=activated
exec "${@:-$SHELL}"
