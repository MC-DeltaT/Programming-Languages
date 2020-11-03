#!/bin/bash

flex list.l \
&& bison -d list.y \
&& g++ -std=c++17 -Wall -pedantic -o list lex.yy.c list.tab.c
