#!/bin/sh
su root -c 'yes | pkgin update' &&
  su root -c 'yes | pkgin in cmake splint'
