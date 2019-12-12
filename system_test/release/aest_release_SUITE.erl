-module(aest_commands_SUITE).

%=== EXPORTS ===================================================================

% Common Test exports
-export([all/0]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

% Test cases
-export([release/1]).

%=== INCLUDES ==================================================================

-include_lib("stdlib/include/assert.hrl").

%=== MACROS ====================================================================

-define(STARTUP_TIMEOUT, 20000).

%=== COMMON TEST FUNCTIONS =====================================================

all() -> [
    release
].

init_per_testcase(_TC, Config) ->
    Config.

end_per_testcase(_TC, Config) ->
    ok.

%=== TEST CASES ================================================================

release(Cfg) ->
    ct:log("Extract package"),
    PackageName = os:getenv("PACKAGE"),
    ?assert(PackageName != false),
    TargetDir = lib:nonl(os:cmd("mktemp -d")),
    {ok, _} = case os:type() of
                  {win32, _} ->
                      zip:extract(PackageName, [cwd, TargetDir])
                  _ ->
                      erl_tar(PackageName, [compressed, {cwd, TargetDir}])
              end,
    ct:log("Copy configuration"),
    ct:log("Start node"),
    ct:log("Verify version"),
    ct:log("Verify status"),
    ct:log("Verify top block"),
    ct:log("Restart node"),
    ct:log("Verify top block"),
    ct:log("Stop node"),
    ct:log("Cleanup"),
    
    ok.

%=== INTERNAL FUNCTIONS ========================================================

