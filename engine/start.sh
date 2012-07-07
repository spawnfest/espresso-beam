#!/bin/sh
erl -sname espresso_beam -pa ebin/ -eval "application:start(espresso_beam)."

