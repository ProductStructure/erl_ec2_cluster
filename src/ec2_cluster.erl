-module('ec2_cluster').

-compile({parse_transform, cut}).
-compile({parse_transform, do}).
-compile({parse_transform, import_as}).

-export([get_nodes_by_tag/1, get_hostname_from_instance_id/1]).

unwrap([X|_]) -> X.

get_hostname_from_instance_id(InstanceId)->
  StateT = state_t:new(identity_m),
  SM     = StateT:modify(_),
  StateT:exec(
    do([StateT ||
        StateT:put(erlcloud_ec2:describe_instances([InstanceId])),
        SM(fun unwrap/1),
        SM(proplists:get_value(instances_set, _)),
        SM(fun unwrap/1),
        SM(proplists:get_value(private_dns_name, _)),
        SM(fun (Name) -> "webmachine@"++Name end),
        SM(fun (Name) -> list_to_atom(Name) end)]), undefined).

%-spec(get_nodes_by_tag(string()) -> [nodes()]).
get_nodes_by_tag(Tag) ->
  {ok, EC2Nodes} = erlcloud_ec2:describe_tags([ {value, [Tag]}]),
  Nodes = [begin
     Node =  get_hostname_from_instance_id(InstanceId),
     net_adm:ping(Node),
     Node
   end || {ec2_tag, InstanceId, _, _ ,_} <- EC2Nodes],
   resource_discovery:trade_resources(),
   Nodes.



