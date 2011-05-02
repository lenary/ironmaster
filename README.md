Ironmaster
==========

An Erlang server management app.

Aims:
-----

The aim of this project in the short term is to have a self-contained erlang
application that can manage complex server setups. Why Erlang? All the rest of
the stack is written in erlang, so we can distribute it more easily, which
although it is not a requirement for Ironmaster, will be very helpful.

Roadmap:
--------
- Server Pools: when upgrading the servers running our erlang apps, we will need
  to spin up another server instance, upgrade it, then move an app instance over
  to it, before upgrading the server instance where that app was and using it.
  This is normal N+1 server pools

- Remote Commands: To upgrade each server instance, we're going to need to run
  commands on them from a local command centre. Seems sensible enough? Defining
  those commands should be fun though.

- Utility functions: No one wants to write complicated commands for trivial
  operations, so we'll work out a few commands for them to do otherwise.
