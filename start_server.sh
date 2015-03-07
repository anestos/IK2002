#!/usr/bin/env bash

ERL=/usr/bin/erl

$ERL -pa ebin/ -pa kdc_server/pbkdf2/ -boot start_sasl -eval "tcp_srv:start_srv()."
