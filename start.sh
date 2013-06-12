#!/usr/bin/env bash

erl +K true -smp enable -pa ebin #-eval "ra_ws:start(1, 3000), sleep(10000)."
