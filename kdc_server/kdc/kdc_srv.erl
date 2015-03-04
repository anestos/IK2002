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
try_read({ok, [Terms]}) ->
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

find_client(Address) ->
	[Terms] = try_read(file:consult("store.dat")),
	Result = lists:keyfind(Address, 3, Terms),
	Result.
	
handler(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Data} ->
			{ok, {Address, Port}} = inet:peername(Socket),
			io:format("Client IP: ~p port: ~p~n", [Address, Port]),
			case find_client(inet_parse:ntoa(Address)) of
				#clients{user_name=Username,
					ip_addr=Ip_addr,
					crypto_key=Key} ->
						io:format("Username: ~p~n", [Username]),
						io:format("IP: ~p~n", [Ip_addr]),
						io:format("Key: ~p~n", [Key]),
						io:format("Clear: ~p~n", [decrypt(Data, Key)]);
				[] -> io:format("User not foun!")
			end,
			gen_tcp:close(Socket),
			handler(Socket);
		{error, closed} -> error
	end.

decrypt(Data, Key) ->
	% First 16 bytes are the IV
	Iv = binary:part(Data, 0, 16),
	% The rest is the cipher
	Cipher = binary:part(Data, 17, byte_size(Data)),
	Clear = crypto:aes_ctr_decrypt(Key, Iv, Data),
	binary_to_list(Clear).
	
	

tester(Address) ->
	#clients{user_name=Un, ip_addr=Ip, crypto_key=C} = find_client(Address),
	io:format("Username: ~p~n", [Un]),
	io:format("IP: ~p~n", [Ip]),
	io:format("Key: ~p~n", [C]),
	C.

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
