-module(ec2c_app).

-behavior(e2_application).

-export([init/0]).

init() ->
    {ok, [ec2c_cluster]}.
