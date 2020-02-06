#!/usr/bin/env bash
# script to abbreviate raster names

for F in * ; do
  NEWNAME=$(echo $F| cut -d_ -f5| sed 's|-||')".""${F#*.}";
  mv $F $NEWNAME ; 
done
