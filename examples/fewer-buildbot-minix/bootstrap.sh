#!/bin/sh
su root -c 'yes | pkgin update && yes | pkgin in cmake splint'
