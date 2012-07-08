#!/bin/sh
erl -sname espresso_beam -pa deps/cowboy/ebin -pa ebin/ -eval "application:start(espresso_beam)."

