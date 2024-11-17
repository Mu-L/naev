#!/bin/bash

set -e

usage() {
    echo "usage: $(basename "$0") [-d] [-n] (set this for nightly builds) -s <SOURCEROOT> (Sets location of source) -b <BUILDROOT> (Sets location of build directory) -r <RUNNER> (must be specified)"
    cat <<EOF
RELEASE SCRIPT FOR NAEV Soon (tm)

This script attempts to compile and build different parts of Naev
automatically in order to prepare for a new release.

Pass in [-d] [-n] (set this for nightly builds) -s <SOURCEROOT> (Sets location of source) -b <BUILDROOT> (Sets location of build directory) -r <RUNNER> (must be specified)

Output destination is ${BUILDPATH}/dist
EOF
    exit 1
}

# Defaults
NIGHTLY="false"

while getopts dns:b:o:r: OPTION "$@"; do
    case $OPTION in
    d)
        set -x
        ;;
    n)
        NIGHTLY="true"
        ;;
    s)
        SOURCEROOT="${OPTARG}"
        ;;
    b)
        BUILDPATH="${OPTARG}"
        ;;
    r)
        RUNNER="${OPTARG}"
        ;;
    *)
        usage
        ;;
    esac
done

if [ -z "$SOURCEROOT" ] || [ -z "$BUILDPATH" ] || [ -z "$RUNNER" ]; then
    usage
fi


function get_version {
   if [ -f "$SOURCEROOT/dat/VERSION" ]; then
       VERSION="$(<"$SOURCEROOT/dat/VERSION")"
       export VERSION
   else
       echo "The VERSION file is missing from $SOURCEROOT."
       exit 1
   fi

   return 0
}

function make_appimage {
   if [[ "$NIGHTLY" == "true" ]]; then
      sh "$SOURCEROOT/utils/buildAppImage.sh" -n -s "$SOURCEROOT" -b "$BUILDPATH/appimage"
   else
      sh "$SOURCEROOT/utils/buildAppImage.sh" -s "$SOURCEROOT" -b "$BUILDPATH/appimage"
   fi
}

function make_windows {
   meson install
}

function make_macos {
   meson install
}

function make_steam {
   if [[ $RUNNER == "Windows" ]]; then
      echo "TODO!"
   elif [[ $RUNNER == "macOS" ]]; then
      echo "Nothing to do!"
   elif [[ $RUNNER == "Linux" ]]; then
      echo "TODO!"
   else
      echo "Invalid Runner name, did you pass in runner.os?"
   fi
}

function make_itch {
   if [[ $RUNNER == "Windows" ]]; then
      echo "TODO!"
   elif [[ $RUNNER == "macOS" ]]; then
      echo "Nothing to do!"
   elif [[ $RUNNER == "Linux" ]]; then
      echo "TODO!"
   else
      echo "Invalid Runner name, did you pass in runner.os?"
   fi
}

# Create output dirdectory if necessary
mkdir -p "$BUILDPATH/dist"

# Build Release stuff
if [[ $RUNNER == "Windows" ]]; then
   make_windows
   make_steam
elif [[ $RUNNER == "macOS" ]]; then
   make_macos
elif [[ $RUNNER == "Linux" ]]; then
   make_appimage
   make_steam
else
   echo "Invalid Runner name, did you pass in runner.os?"
fi
