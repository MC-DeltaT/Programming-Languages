#!/bin/bash

# For some reason the `-static` linking option is required otherwise the program doesn't do anything.
f77 -o fizzbuzz -static fizzbuzz.f
