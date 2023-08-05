
{-# LANGUAGE OverloadedStrings #-}

module Tools where

import Shh (Command)
import Shh.Internal (toArgs)

cat :: Command a => a
cat = toArgs ["cat"]

date :: Command a => a
date = toArgs ["date"]

echo :: Command a => a
echo = toArgs ["echo"]

sleep :: Command a => a
sleep = toArgs ["sleep"]

curl :: Command a => a
curl = toArgs ["curl"]

cardano_cli :: Command a => a
cardano_cli = toArgs ["cardano-cli"]

marlowe_cli :: Command a => a
marlowe_cli = toArgs ["marlowe-cli"]

marlowe_runtime_cli :: Command a => a
marlowe_runtime_cli = toArgs ["marlowe-runtime-cli"]
