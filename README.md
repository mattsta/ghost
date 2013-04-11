ghost: generalized hierarchical object storage
==============================================
(plus, nose: New Object Storage Engine)

Status
------
ghost and nose form a redis-backed hierarchical
object storage system.  What good is hierarchical storage? Well,
we can use it to make threaded discussion systems, trees of users,
or other things that contain other things that contain
other things.

Usage
-----
Usage can be complex.  For now, see tests for examples.

Building
--------
        rebar get-deps
        rebar compile

Testing
-------
The tests need a local redis server.  See the test modules for
a redis port to use (or change what you need to get things working).
The tests will FLUSHALL on your redis server, so set up a dummy
non-disk-backed instance for easy tests.

        rebar eunit skip_deps=true suite=ghost
        rebar eunit skip_deps=true suite=nose

Next Steps
----------
Release full example applications.
