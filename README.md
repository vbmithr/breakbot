# Breakbot — An OCaml bot for arbitraging Bitcoin exchanges

Breakbot is a bot able to take advantage of price discrepancies
between several bitcoin exchanges. So far, support has been added for
MtGox, Bitstamp, BTC-e and a limited support for Intersango has been
added as well.

I’m releasing this code because this is currently not possible
(understand: profitable) to perform arbitrage between bitcoins
exchanges. The main problem currently (2012-12-14) is that there is no
way to automatically move funds (in currency equivalent, like USD)
between exchanges (or the cumulative fees of buying/selling/moving
money around would then be to high), and that the profit realized in
performing arbitrage is currently too low to justify doing this
activity manually. There is also no reason to believe it’s going to be
so in a foreseeable future.

## Features

* Exchange info is fed using the fastest possible way (websockets for
  MtGox, TCP socket for Intersango, HTTP polling for the other ones
  that do not support anything better).

* CLI to interact with several bitcoin exchanges

## Dependencies

* lwt
* cohttp
* ocaml-rpc
* bitstring
* jsonm
* ocaml-websocket
* uuidm
* cmdliner
* zarith

I recommand you install these libraries using OPAM.

## Install

Just type `make`. It will produce two executables, `cli.native` and
`breakbot.native`. The CLI is a command-line interface to the
supported bitcoin exchanges, and it allows to interact with your
account in several ways (mainly display balance, place an order,
withdraw bitcoins, display ticker, etc…). `breakbot.native` is the bot
itself, currently all is he doing is to print out arbitrage
opportunities each time an exchange is updated, that is, printing the
amount of money to be moved from one exchange to another, and the
resulting gain you can expect by doing so. Transaction fees for the
exchanges are hardcoded, and are set to the current (2012-12-14)
highest possible fees of a given exchange. It is thus an upper bound.

## Configuration

Rename `breakbot.conf.example` into `breakbot.conf` and replace the
template strings by actual values. The file follows the JSON syntax.

## Going further

This is not a final product, I abandonned development when I realized
that this project cannot be profitable for now. It was a cool project
to develop, but as it misses its purpose, there is no much point doing
anything more for it for now. Maybe the CLI could be useful for people
that frequently interact with bitcoin exchanges, but it would require
some polish to be really practical to use (and probably some testing
as well :)
