#!/bin/bash
cd source
erl -eval 'yecc:file("minierlang.yrl").' -s init stop -noshell
