#!/bin/bash

rebar get-deps compile

if [ ! -d var ]; then
  mkdir var
  cp priv/app.config.default var/app.config
fi

exec erl -sname im -config var/app.config -pa ebin -s ironmaster
