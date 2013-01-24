#!/bin/sh

ozc -c Compile.oz && ozc -c Test.oz && ozengine Test.ozf
