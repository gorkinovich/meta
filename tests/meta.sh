#!/bin/bash
erl -pa ../out/production/meta -eval "meta:main(\"$1\")." -s init stop -noshell
