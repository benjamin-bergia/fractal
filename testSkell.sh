#!/bin/bash
sourceDir="./src/"
sourceFile=$sourceDir$1".erl"
testDir="./test/"
testFile=$testDir$1"_tests.hrl"
echo -e "Reading:\t$sourceFile"
echo -e "Creating:\t$testFile"
grep '^.*(.*) ->' $sourceFile > $testFile
