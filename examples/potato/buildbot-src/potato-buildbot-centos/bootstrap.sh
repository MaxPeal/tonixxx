#!/bin/bash
set -eEuo pipefail

sudo yum update -y

sudo yum install -y epel-release

sudo yum install -y valgrind

dub fetch dscanner

dub fetch dale
