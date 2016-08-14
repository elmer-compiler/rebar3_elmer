rebar3_elmer
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { rebar3_elmer, ".*", {git, "git@host:user/rebar3_elmer.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 rebar3_elmer
    ===> Fetching rebar3_elmer
    ===> Compiling rebar3_elmer
    <Plugin Output>
