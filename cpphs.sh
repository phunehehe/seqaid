#!/usr/bin/env bash

# (Hopefully that works on mingw32.)

## #!/bin/sh

# This is mostly to get rid of rogue failed build reports.
# Not my fault I have to make things ugly to fix mistakes of
# others (build reporters with misconfigured environemnts; and
# hackage, for allowing this to smear a perfectly good library
# by displaying the report so prominently, and making it
# impossible for the maintainer to get rid of it or even
# add an annotation).  I could have been doing something
# constructive with this time.

# (Still not enough, if you build in the sandbox with multiple GHC versions?)
env PATH=.cabal-sandbox/bin:$HOME/.cabal/bin:/home/builder/hackage-server/build-cache/tmp-install/bin:$PATH cpphs --cpp $*

if [ 0 ]; then

if [ -f .cabal-sandbox/bin/cpphs ]; then
  # Building in a sandbox, you want the sandbox's cpphs:
  .cabal-sandbox/bin/cpphs --cpp $*
elif [ $(which cpphs) ]; then
  # Either cpphs was already on your path, or it was just installed and
  # is seen on your path -- use this one:
  cpphs --cpp $*
elif [ -f $HOME/.cabal/bin/cpphs ]; then
  $HOME/.cabal/bin/cpphs --cpp $*
elif [ $(which cpp) ]; then
  cpp $*
else
  echo 'ERROR: Neither cpphs nor cpp could be found!'
  exit 1
fi

fi

