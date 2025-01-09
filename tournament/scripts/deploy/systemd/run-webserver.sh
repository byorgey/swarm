#!/bin/bash -xe

./tournament-bin --version $(cat git-hash.txt) --port 5500
