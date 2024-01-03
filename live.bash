#!/usr/bin/env bash
elm-live \
  --verbose \
  --proxy-prefix=/api \
  --proxy-host=https://materiamq.eu-fr-1.services.clever-cloud.com:443 \
  src/Main.elm
