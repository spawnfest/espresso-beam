#!/bin/sh
erl -sname espresso_beam -pa engine/ebin -eval "application:start(espresso_beam)."

