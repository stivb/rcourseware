#!/bin/bash

# Recursively find and delete directories named "g" followed by a number
find . -type d -regex '.*/g[0-9]+' -exec rm -rf {} +