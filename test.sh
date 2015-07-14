#!/bin/bash

export JS_HOME=$PWD
bin/jsaf analyze -result sample.out sample.js | tee vectors.txt

