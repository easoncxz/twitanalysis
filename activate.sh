#!/usr/bin/env bash

# This environment is useful only for running the Jekyll build for
# the Github Pages site of this Github repo. None of this have anything
# to do with actual TwitAnalysis code, frontend or backend.

# https://blog.jez.io/ruby-virtualenvs/
export GEM_HOME=.gems
export GEM_PATH=

# For a .env file oneline-interpreter in Bash:
# https://gist.github.com/judy2k/7656bfe3b322d669ef75364a46327836

# For a better `activate` scripts: (from 2012)
# https://datagrok.org/python/activate/
export VIRTUAL_GEM_ENV=activated

echo "Activating new shell with VIRTUAL_GEM_ENV=activated and GEM_HOME=${GEM_HOME}"
echo "Deactivate by exiting the shell, e.g. by typing 'exit' or ^D"
exec "${@:-$SHELL}"
