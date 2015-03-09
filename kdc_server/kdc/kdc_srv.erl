-module(kdc_srv).
-export([add_client/2, handler/1]).

-define(STORAGE, "store.dat").

-record(clients, {user_name = username,
		ip_addr = ipAddr,
		crypto_key = cryptoKey}).

-spec add_client(Username, Ipaddr) -> no_return() when
	Username :: string(),
	Ipaddr :: string().

add_client(Username, Ipaddr) ->
	Key = gen_key(Ipaddr),
	Client = #clients{user_name = Username,
			ip_addr = Ipaddr,
			crypto_key = Key},
	write2file(Client).

-spec write2file(Client) -> no_return() when
	Client :: string().

write2file(Client) ->
	Terms = try_read(file:consult(?STORAGE)),
	Tmp = [Client | Terms],
	unconsult(?STORAGE, Tmp).

-spec unconsult(File, L) -> ok | {error, Reason} when
	File :: string(),
	L :: [term()],
	Reason :: file:posix() | badarg | terminated.

unconsult(File, L) ->
	{ok, S} = file:open(File, write),
	lists:foreach(fun(X) -> io:format(S, "~p.~n",[X]) end, L),
	file:close(S).

-spec try_read(Terms) -> [] when
	Terms :: {error, Reason} | {ok, [term()]},
	Reason :: file:posix() | badarg | terminated | system_limit.

try_read({error, _Reason}) ->
	[];
try_read({ok, Terms}) ->
	Terms.

-spec gen_key(Ipaddr) -> binary() when
	Ipaddr :: string().

gen_key(Ipaddr) ->
	Password = getpass(),
	{Salt, Iterations, DerivedLength} = {list_to_binary(Ipaddr), 4000, 32},
	{ok, Key} = pbkdf2:pbkdf2(sha, Password, Salt, Iterations, DerivedLength),
	Key.

-spec gen_session_key() -> binary().
	
gen_session_key() ->
	{A,B,C} = now(),
	random:seed(A,B,C),
	Password = generate_password(10),
	{Salt, Iterations, DerivedLength} = {list_to_binary(generate_password(5)), 4000, 32},
	{ok, Key} = pbkdf2:pbkdf2(sha, Password, Salt, Iterations, DerivedLength),
	KeyEnc = base64:encode(Key),
	KeyEnc.

-spec generate_password(integer()) -> [term()].

generate_password(N) ->
    lists:map(fun (_) -> random:uniform(90)+$\s+1 end, lists:seq(1,N)).

-spec getpass() -> string().

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

-spec find_client_addr(Address) -> tuple() | false when
	Address :: [any()].

find_client_addr(Address) ->
	Terms = try_read(file:consult(?STORAGE)),
	Result = lists:keyfind(Address, 3, Terms),
	Result.

-spec find_client_addr_name([Peer_name]) -> tuple() | false when
	Peer_name :: any().

find_client_addr_name([Peer_name]) ->
	Terms = try_read(file:consult(?STORAGE)),
	Result = lists:keyfind(Peer_name, 2, Terms),
	Result.

-spec handler(Socket) -> string() | error when
	Socket :: gen_tcp:socket().

handler(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Data} ->
			{ok, {Address, _Port}} = inet:peername(Socket),
			case find_client_addr(inet_parse:ntoa(Address)) of
				#clients{user_name=Username,
					ip_addr=Ip_addr,
					crypto_key=Key} ->
						Clear = decrypt(Data, Key),
						{Nonce, Msg} = get_message(Clear),
						Msg1 = binary_to_list(Msg),
						{User_name, Peer_name} = extract_msg(Msg1),
						case User_name of
							Username ->
								#clients{ip_addr = Peer_ip,
									crypto_key = Peer_key} = 
									find_client_addr_name(Peer_name),
								Peer_session_key = gen_session_key(),
								Ticket = construct_ticket(Username,
									Ip_addr, Peer_session_key, Peer_key),
								Reply = construct_reply(Nonce,
									Peer_name, Peer_ip, Peer_session_key, Ticket, Key),
								gen_tcp:send(Socket, Reply);
							_ -> io:format("User not found~n")
						end;


				false -> io:format("User not found in storage!")
			end,
			gen_tcp:close(Socket),
			handler(Socket);
		{error, closed} -> error
	end.

-spec construct_ticket(Username, Ip_addr, Session_key, Peer_key) -> Bin when
	Username :: string(),
	Ip_addr :: string(),
	Session_key :: binary(),
	Peer_key :: binary(),
	Bin :: [binary()].

construct_ticket(Username, Ip_addr, Session_key, Peer_key) ->
	Clear = [Username,"|", Ip_addr,"|", Session_key],
	{Iv, Cipher} = encrypt(list_to_binary(Clear), Peer_key),
	[Iv, Cipher].

-spec construct_reply(Nonce, Peer_name, Peer_ip, Peer_session_key, Ticket, Key) ->
	Bin when
	Nonce :: binary() | maybe_improper_list(any(),binary() | []) | byte(),
	Peer_name :: binary() | maybe_improper_list(any(),binary() | []) | byte(),
	Peer_ip :: binary() | maybe_improper_list(any(),binary() | []) | byte(),
	Peer_session_key :: binary() | maybe_improper_list(any(),binary() | []) | byte(),
	Ticket :: binary() | maybe_improper_list(any(),binary() | []) | byte(),
	Key :: _,
	Bin :: [binary()].

construct_reply(Nonce, Peer_name, Peer_ip, Peer_session_key, Ticket, Key) ->
	Clear = [Nonce,"|", Peer_name,"|", Peer_ip,"|", Peer_session_key,"|", Ticket],
	Clear2 = list_to_binary(Clear),
	{Iv, Cipher} = encrypt(Clear2, Key),
	[Iv, Cipher].

-spec extract_msg(string()) -> tuple().

extract_msg(Msg) ->
	[User_name | Peer_name] = string:tokens(Msg,"|"),
	{User_name, Peer_name}.

-spec decrypt(binary(), binary()) -> binary().

decrypt(EncData, Key) ->
	% First 16 bytes are the IV
	% The rest is the cipher
	Data = base64:decode(EncData),
	Data_size = bit_size(Data),
	Cipher_size = Data_size - 128,
	<<Iv:128/bitstring, Cipher:Cipher_size/bitstring>> = Data,
	crypto:aes_ctr_decrypt(Key, Iv, Cipher).

-spec encrypt(Data, Key) -> {IvEnc, CipherEnc} when
	Data :: iolist() | binary(),
	Key :: iolist() | binary(),
	IvEnc :: binary(),
	CipherEnc :: binary().

encrypt(Data, Key) ->
	Iv = crypto:rand_bytes(16),
	Cipher = crypto:aes_ctr_encrypt(Key, Iv, Data),
	IvEnc = base64:encode(Iv),
	CipherEnc = base64:encode(Cipher),
	{IvEnc, CipherEnc}.

-spec get_message(binary()) -> {binary(), binary()}.

% First 8 bytes (96bytes b64 encoded) of payload are the nonce
get_message(<<Nonce:96/bitstring, Msg/bitstring>>) ->
	{Nonce, Msg}.
