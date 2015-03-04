-module(tcp_srv).
-export([start_srv/2, stop_srv/1]).
%Only for tests
-export([handler/1]).

-define(MAX_INSTANCES, 20).
-define(PACKET_SIZE_LENGTH, 4).
-define(PACKET_FORMAT, binary).

% Name to register process
-spec srv_register_name(Port) -> ServerName when
	Port :: integer(),
	ServerName :: atom().

srv_register_name(Port) ->
	list_to_atom("SecIM_server" ++ integer_to_list(Port)).

% Entry point
-spec start_srv(Port, fun((Socket) -> no_return())) -> atom() when
	Port :: integer(),
	Socket :: gen_tcp:socket().

start_srv(Port, Fun) when is_integer(Port)->
	Reg_name = srv_register_name(Port),
	%io:format("start_srv~n"),
	spawn_proc(whereis(Reg_name), Fun, Port, Reg_name).

% Stop server
-spec stop_srv(integer()) -> no_return().

stop_srv(Port) when is_integer(Port) ->
	halt_srv(whereis(srv_register_name(Port)), srv_register_name(Port)).

-spec halt_srv('undefined' | pid() | port(), atom()) -> 'ok' | 'stop'.

halt_srv(undefined, _Reg_name) ->
	io:format("No server running!~n");
halt_srv(Pid, Reg_name) ->
	exit(Pid, kill),
	(catch unregister(Reg_name)),
	stop.

-spec spawn_proc('undefined' | pid() | port(), Fun, Port, Reg_name) -> 
	'ok' | 'true' | {'error', 'server_running'} when
	Fun :: fun((Socket) -> no_return()),
	Socket :: gen_tcp:socket(),
	Port :: integer(),
	Reg_name :: atom().

spawn_proc(undefined, Fun, Port, Reg_name) ->
	Self = self(),
	Pid = spawn_link(fun() -> start(Self, Port, Fun) end),
	receive
		{ok, Pid} ->
			%io:format("Spawn_proc OK~n"),
			register(Reg_name, Pid);
		{error, Reason} ->
			io:format("Spawn_proc error: ~p~n", [Reason])
	end;
spawn_proc(_Pid, _Fun, _Port, _Reg_name) ->
	{error, server_running}.

-spec start(Supervisor, Port, Fun) -> {'error', atom()} when
	Supervisor :: pid(),
	Port :: integer(),
	Fun :: fun((Socket) -> no_return()),
	Socket :: gen_tcp:socket().

start(Supervisor, Port, Fun) ->
	% Catch exit signal
	process_flag(trap_exit, true),
	case gen_tcp:listen(Port, [?PACKET_FORMAT,
				{packet, ?PACKET_SIZE_LENGTH},
                                {reuseaddr, true},
                                {active, false}]) of
		{ok, Listen} ->
			io:format("TCP server started on port ~p~n", [Port]),
			Supervisor ! {ok, self()},
			% Accepting
			New_instance = socket_accept(Listen, Fun),
			% Socket controller
			socket_control(Listen, New_instance, [], Fun);
		{error, Reason} ->
			Supervisor ! {error, Reason}
	end.

% Start of socket controller
-spec socket_control(Listen, New_instance, Alive, Fun) -> 'ok' when
	Listen :: gen_tcp:socket(),
	New_instance :: pid(),
	Alive :: list(),
	Fun :: fun((Socket) -> no_return()),
	Socket :: gen_tcp:socket().

socket_control(Listen, New_instance, Alive, Fun) ->
	receive
		{started, New_instance} ->
			Alive1 = [New_instance|Alive],
			start_new_process(false, Listen, Alive1, Fun);
		{'EXIT', New_instance, _Why} ->
			%io:format("Child process is dead: ~p~n",[Why]),
			start_new_process(false, Listen, Alive, Fun);
		{'EXIT', From_pid, _Why} ->
			%io:format("Child process is dead: ~p~n",[Why]),
			Alive1 = lists:delete(From_pid, Alive),
			start_new_process(New_instance, Listen, Alive1, Fun);
		{children, From} ->
			From ! {session_server, Alive},
			socket_control(Listen, New_instance, Alive, Fun);
		Other ->
			io:format("Received something weird: ~p~n", [Other])
	end.

-spec start_new_process(New_instance, Listen, Alive, Fun) -> no_return() when
	New_instance :: pid(),
	Listen :: gen_tcp:socket(),
	Alive :: list(),
	Fun :: fun((Socket) -> no_return()),
	Socket :: gen_tcp:socket().

start_new_process(New_instance, Listen, Alive, Fun) when is_pid(New_instance) ->
	socket_control(Listen, New_instance, Alive, Fun);
start_new_process(false, Listen, Alive, Fun) ->
	case length(Alive) of
		Length when Length < ?MAX_INSTANCES ->
			New_instance = socket_accept(Listen, Fun),
			socket_control(Listen, New_instance, Alive, Fun);
		_ ->
			socket_control(Listen, false, Alive, Fun)
	end.

% End of socket controller
-spec socket_accept(Listen, Fun) -> no_return() when
	Listen :: gen_tcp:socket(),
	Fun :: fun((Socket) -> no_return()),
	Socket :: gen_tcp:socket().

socket_accept(Listen, Fun) ->
	Self = self(),
	spawn_link(fun() -> start_listen_socket(Self, Listen, Fun) end).

-spec start_listen_socket(Controller, Listen, Fun) -> no_return() when
	Controller :: pid(),
	Listen :: gen_tcp:socket(),
	Fun :: fun((Socket) -> no_return()),
	Socket :: gen_tcp:socket().

start_listen_socket(Controller, Listen, Fun) ->
	apply_socket(Controller, gen_tcp:accept(Listen), Fun).

-spec apply_socket(Controller, Socket, fun((Socket) -> no_return())) ->
	no_return() when
	Controller :: pid(),
	Socket :: gen_tcp:socket().

apply_socket(Controller, {ok, Socket}, Fun) ->
	% Inform controller
	%io:format("Inform controller~n"),
	Controller ! {started, self()},
	Fun(Socket);
apply_socket(_Controller, {error, Reason}, _Fun) ->
	io:format("Something wrong happened: ~p~n", [Reason]),
	exit(dead).

