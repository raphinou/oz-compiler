#!/bin/sh

ozc -c Compile.oz -o ../lib/Compile.ozf  && ozc -c Run.oz && ozengine Run.ozf
