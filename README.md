IDEA EGTM: Erlang binding for GT.M database engine
==================================================

Software product description and documentation for application
developers is to be found on [http://labs.idea.cz/egtm](
http://labs.idea.cz/egtm "IDEA EGTM Technology").

Installation
------------
* `git clone http://github.com/ztmr/egtm`
* ensure the latest Erlang/OTP is installed
* ensure GT.M is installed
* change GT.M `gtm_dist` path in `priv/gtmenv` and `rebar.config`
* `./rebar get-deps && ./rebar compile`
* initialize database with `./priv/initdb`
* run the console `./priv/egtm_console`
* enjoy!

Notes
-----
* **GT.M compatibility**
  - EGTM is known to not work with GT.M V5.5-000 Linux x86\_64
    because of [call-in bug](https://groups.google.com/d/topic/comp.lang.mumps/R_GvkUUZaq0/discussion "Call-in bug").
* **performance:**
  - per-operation lager debug logging (disabled by default)
    means around ~2000 microseconds overhead,
  - metrics (disabled by default) costs around
    ~30 additional microseconds,
  - both can be enabled using `EGTM_METRICS` and
    `EGTM_TRACE` compile-time macros,
  - unless `EGTM_METRICS` is defined, the `egtm_metrics`
    configuration (in `priv/egtm.conf`) is ignored.
* **transaction processing:**
  - since *GT.M call-in interface does not allow us to call
    standalone `TS`/`TC`/`TRO` commands*, we had to implement
    TP emulation. This is done by storing all write/lock operations
    within a virtual transaction buffer that is evaluated
    at the time of commit.
  - this has *some limitations*, for example:
    <pre>
    %% Expect the ^ZTMR to hold value of 0.
    DataProcessing = fun () ->
    &nbsp;&nbsp;io:put\_chars (egtm:get ("^ZTMR")),
    &nbsp;&nbsp;egtm:set ("^ZTMR", 1), %% when in transaction, this is not done immediatelly but at the commit-time!!!
    &nbsp;&nbsp;io:put\_chars (egtm:get ("^ZTMR")),
    &nbsp;&nbsp;false   %% Rollback!
    end,
    egtm\_util:transaction (DataProcessing), %% outputs 00{ok,rollback,unknown}
    egtm\_util:transaction (DataProcessing), %% outputs 00{ok,rollback,unknown}
    DataProcessing (),                      %% outputs 01false
    DataProcessing ().                      %% outputs 11false
    </pre>
  - *operations supported by TP*:
    `set`, `setp`, `kill`, `zkill`, `lock`, `unlock`.
  - To get *full-featured GT.M TP*, feel free to *write your own*
    `priv/rtns/MyRoutine.m` with any TP processing code and
    call it using `egtm:do/2` or `egtm:call/2`.
* **clustering and multi-site configurations:** EGTM HAC
  is a separate and paid add-on product.
* **consulting and support services, non-public/non-free add-ons**:
  Support and consulting services and non-public add-ons may
  be delivered individually under conditions specified in
  a valid support/license contract in context of one or more
  of these products:
  - FIS GT.M and FIS PIP/DATA-QWIK framework,
  - Erlang and ChicagoBoss framework,
  - IDEA EGTM,
  - IDEA Object Database (IODB),
  - IDEA High-Available Cluster (EGTM HAC),
  - IDEA CloudOS (ICOS).
  Feel free to contact IDEA Systems via e-mail or
  [www.idea.cz](http://www.idea.cz "IDEA Systems")
  in any case of interest!

TODO
----
* NIF upgrade/reload functinality -- GT.M call-ins
  are limited to a single `gtm_init`/`gtm_exit` call
  per each process, so we simply cannot unload GT.M
  shared library and load its new version gracefully :-(
* documentation: some `egtm` functions exists in more
  overloaded variants resolved by guards and well
  documented in source code itself.
  The standard edoc generator works only with the first
  match of all these variants, so the resulting HTML
  documentation may not be complete! :-(
* `init^%egtmapi` and error trapping
* `egtm:lock` timeout support
* EGTM NIF stores strings in static char array limited
  by `EGTM$BUFLEN` constant although each string has
  different lenght requirements on different places.
  This should be changed in future releases!
  Use the following:
  - `MAXCODE = 8192`
    (maximum length of a line of code for
    the compiler / variable name)
  - `MAXMSG = 2048`
    (maximum length of a GT.M message)
  - `MAXNAME = 32`
    (one more than the maximum length of a GT.M name)
  - `MAXSTR = 1048576`
    (maximum length of a value that GT.M can return)
* UTF-8 or Erlang-agnostic Latin-1 support without complicated
  encoding/decoding!!
  This is partially supported by `egtm_string` `encode`/`decode`,
  but currenly only for VALUES in `set`/`setp` and `get`/`getp`,
  thus NOT for: global names, subscripts, `do`/`call` arguments.
* Counters for intercluster load distribution purposes
  as well as for a common SNMP monitoring purposes.
  SNMP data may be based on Folsom metrics.
* WebAdmin (at least global browser) -- as ICOS Application?


Architecture Schema Design
--------------------------
<pre>
......................................
: Erlang/OTP Application Server #1   :
:                                    :..
:  +----------------------------+      :
:  | Application that uses EGTM |      :
:  +--------------+-------------+      :
:                 |                    :
:    +------------+-----------------+  :
:    | EGTM Master Broker Server    |  :
:    |..............................|  :
:    | does request routing logic   |  :
:    | based on deployment setup    | (A) single standalone worker
:    | (standalone, pool, cluster)  | (B) pool of local workers
:    |..............................| (C) cluster of A/B-mode servers
:    |      (A)(B)(C)               |  :
:    +-------+--+--+----------------+  :
:           /   |   \                  :.......
:          /    |    \                        :
:         /    /   +--+--------------------+  :
:        /    /    | EGTM Cluster Manager  |  :
:       /    /     | with IntelliRoute     |  :
:      /    /      +----------+------------+  :
:     /    /                  | ..............:
:    /     |        +---------+------------+
:   /      |        |          .:          |
:   |   +--+--------+-------+  :   +-------+---------------------+
:   |   | EGTM Worker Pool  |  :   | EGTM Cluster Neighbour Pool |
:   |   | egtm1, egtm2, ... |  :   +---------+-------------------+
:   |   +-----------+-------+  :           |
:   |               |  ........:           |
:   |               |  :                   |
:   |               |  :   +---------------+-------------+
: +--+----------+   |  :   | EGTM Slave Broker Server    |
: | Standalone  |   |  :   | another worker/pool/cluster |
: | EGTM Worker |   |  :   | SCHEMA RECURSION GOES HERE  |
: +------+------+   |  :   +---------------+-------------+
:        |          |  :                   |
:  +-----+--------+-+  :                +--+------------+
:  | GT.M master DB |===(replication)===| GT.M slave DB |
:  +----------------+  :                +---------------+
:......................:
</pre>


Licensing
=========
Copyright (C) 2012 Tomas Morstein, IDEA Systems

This program is free software: you can redistribute
it and/or modify it under the terms of the GNU Affero
General Public License as published by the Free Software
Foundation, either version 3 of the License,
or (at your option) any later version.

This program is distributed in the hope that it will
be useful, but WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU Affero General
Public License for more details.

You should have received a copy of the GNU Affero
General Public License along with this program.
If not, see [http://www.gnu.org/licenses/](
http://www.gnu.org/licenses/ "GNU Licensing Overview").
