-module(ec2c_find_leader).

-export([get_nodes/0]).
-compile(export_all).
-export([start_link/1, handle_task/1]).
-behavior(e2_task).
-spec(get_nodes() -> [node()]).


get_nodes() ->
  lists:sort(nodes() ++ [node()]).


-spec(get_first_node() -> {ok, node()}).
get_first_node() ->
  [First|_] = get_nodes(),
  {ok, First}.


-spec(is_first_node() -> boolean()).
is_first_node() ->
  {ok, First} = get_first_node(),
  First =:= node() .


-spec(load_apps([atom()]) ->[atom]).
load_apps(Applications) ->
  [begin
    ok = application:load(App),
   App
   end|| App <- Applications].


-spec(enable_app(boolean(), atom()) -> atom()).
enable_app(State,Application) ->
  ok = application:permit(Application, State),
  case State of 
    true -> resource_discovery:add_local_resource_tuple({Application, node()});
    false -> ok
  end,
  ok.


validate_resource_is_alive(ResourceName) ->
  Resources = resource_discovery:get_resources(ResourceName),
  [
      case net_adm:ping(R) of
        pang -> 
          resource_discovery:delete_resource_tuple({ResourceName,R}),
          [];
        pong -> R
      end
      || R<- Resources].


resource_is_avalable(ResourceName) ->
  case validate_resource_is_alive(ResourceName) of
    []     -> enable_app(is_first_node(), ResourceName);
    [_Node] -> ok
  end.


start_link(_) ->
  {ok,Apps}  = application:get_env(applications),
  resource_discovery:add_target_resource_types(Apps),
  load_apps(Apps),
  handle_task(Apps),
  resource_discovery:trade_resources(),
  e2_task:start_link(?MODULE, Apps, [{repeat, 30000}]).


handle_task(Apps) ->
  [resource_is_avalable(App) || App <- Apps],
  {repeat,Apps }.

