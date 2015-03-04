-module(kdc_srv).
-compile(export_all).

-record(clients, {user_name = username,
		ip_addr = ipAddr,
		crypto_key = cryptoKey}).

add_client(Username, Ipaddr) ->
	Key = gen_key(Username, Ipaddr),
	Client = #clients{user_name = Username,
			ip_addr = Ipaddr,
			crypto_key = Key},
	io:format("Username: ~p~n", [Username]),
	io:format("Ipaddr: ~p~n", [Ipaddr]),
	io:format("Key: ~p~n", [Key]),
	write2file(Client).

write2file(Client) ->
	Terms = try_read(file:consult("store.dat")),
	io:format("Terms: ~p~n", [Terms]),
	Tmp = lists:append(Terms, [Client]),
	file:write_file("store.dat", io_lib:fwrite("~p.\n", [Tmp])).


try_read({error, _Reason}) ->
	[];
try_read({ok, Terms}) ->
	Terms.

add2list([List], Client) ->
	[Client | List].


gen_key(Username, Ipaddr) ->
	Password = getpass(),
	{Salt, Iterations, DerivedLength} = {list_to_binary(Ipaddr), 4000, 32},
	{ok, Key} = pbkdf2:pbkdf2(sha, Password, Salt, Iterations, DerivedLength),
	Key.
	
getpass() ->
    % Store current options for stdio.
    InitialIOOpts = io:getopts(),
    % Disable input character echo.
    ok = io:setopts([{echo, false}]),
    % Prompt the user for a password.
    EnteredPassword = io:get_line("Password: "),
    % Restore original options for stdio.
    ok = io:setopts(InitialIOOpts),
    % Print a newline, since we had local echo disabled above.
    io:format("\n"),
    % Remove trailing newline character, if present.
    case lists:reverse(EnteredPassword) of
        [$\n | Rest] ->
            lists:reverse(Rest);
        _ ->
            EnteredPassword
    end.

handler(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Data} ->
			{ok, {Address, Port}} = inet:peername(Socket),
			io:format("Client IP: ~p port: ~p~n", [Address, Port]);
		{error, closed} -> error
	end.


% Input handler test case
ihandler(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Data} ->
			printer(binary_to_list(Data)),
			gen_tcp:close(Socket),
			handler(Socket);
		{error, closed} -> ok
	end.

printer(Data) ->
	%io:format("Data size: ~p~n", [bit_size(A)]),
	case Data of
		"2" -> 
			io:format("Data received: 2~n");
		"3" ->
			io:format("Going to sleep~n"),
			timer:sleep(10000),
			io:format("Data received: 3~n");
		Other ->
			io:format("Something: ~p~n", [Other])
	end.
