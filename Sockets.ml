open Unix

type os = Apple | Other

let find_port fd = 
  begin
    match getsockname fd with
    | Unix.ADDR_INET (_, port) -> port
    | _ -> failwith "whatever"
  end

let echo s =
  let code = match system ("echo '" ^ s ^ "'") with 
    | WEXITED c -> c
    | _ -> failwith "Process killed/stopped" in
  if code <> 0 then failwith ("Failed with code " ^ string_of_int code)
  else ()

let env () = 
  match getenv "TERM_PROGRAM" with
  | "Apple_Terminal" -> Apple
  | _ -> Other

let listen_accept fd =
  listen fd 1;
  echo "Please give your IP Address and this port number to your opponent: \n";
  echo "Your IP Address: ";
  let code = 
    (if env () = Apple then system "ifconfig en0 | grep broadcast | grep -o 'inet\ [0-9]*\\.[0-9]*\\.[0-9]*\\.[0-9]*' | grep -o '[0-9]*\\.[0-9]*\\.[0-9]*\\.[0-9]*'"
     else system "ifconfig eth0 | grep broadcast | grep -o 'inet\ [0-9]*\\.[0-9]*\\.[0-9]*\\.[0-9]*' | grep -o '[0-9]*\\.[0-9]*\\.[0-9]*\\.[0-9]*'")
  in
  if code = WEXITED 0 then (echo ("Port number: \n" ^ string_of_int (find_port fd) ^ "\n");)
  else (echo "We couldn't find your IP Address. You will have to find it manually.\n");
  echo "Waiting for opponent to connect...";
  let (conn_fd, sockaddr) = Unix.accept fd in
  let msg = Bytes.create 64 in
  let msg = match Unix.recv conn_fd msg 0 64 [] with
    | len -> Bytes.of_string (String.sub (Bytes.to_string msg) 0 len)
  in
  print_string (Bytes.to_string msg);;


let init_with_socket_host () =
  listen_accept (socket PF_INET SOCK_STREAM 0)

let conn fd = 
  print_string "Please enter your opponent's IP Address:\n";
  let ip = read_line () in
  print_string "Please enter the port number to connect to on your opponents machine:\n";
  let port = read_line () in
  let conn_addr = Unix.ADDR_INET(Unix.inet_addr_of_string ip,int_of_string port) in
  Unix.connect fd conn_addr;
  print_string "Enter a short message for me to see: ";
  let msg = read_line () in
  let code = Unix.send_substring fd (msg) 0 (String.length msg) [] in
  if code = code then ()

let init_with_socket_client () =
  conn (socket PF_INET SOCK_STREAM 0)

