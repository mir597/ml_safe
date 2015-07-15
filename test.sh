#!/bin/bash

export JS_HOME=$PWD
bin/jsaf analyze -result benchmarks/icse2013/callgraphs/3dmodel/dynamic-cg.fixed.json benchmarks/icse2013/callgraphs/3dmodel/*.js | tee vectors.txt

