#!/bin/bash
cd tests
erl -pa ../out/production/meta -noshell -run meta main -s init stop -extra $*
cd ..
