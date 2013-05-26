#!/bin/sh
erl -pa ebin deps/*/ebin -s stats -sname server

