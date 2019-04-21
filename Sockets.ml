open Unix
open State
open Command
open Pervasives

type os = Apple | Other

let sto64b s = 
  let b = Bytes.make 64 (String.get " " 0) in
  Bytes.blit_string s 0 b 0 (String.length s);
  b

let rec write_children f_list s =
  match f_list with 
  | [] -> ()
  | h::t -> begin match write (snd h) (sto64b s) 0 64 with
      | 64 -> ()
      | _ -> failwith "64 bytes should have been sent"
    end;
    write_children t s

let find_port fd = 
  begin
    match getsockname fd with
    | Unix.ADDR_INET (_, port) -> port
    | _ -> failwith "whatever"
  end

let env () = 
  match getenv "TERM_PROGRAM" with
  | "Apple_Terminal" -> Apple
  | exception Not_found -> Other
  | _ -> Other

let receive fd =
  ANSITerminal.(print_string [red] "Waiting for your opponent to make a move\n"); 
  Pervasives.print_newline ();
  let msg = Bytes.create 64 in
  match recv fd msg 0 64 [] with
  | len -> Bytes.of_string (String.sub (Bytes.to_string msg) 0 len)

let spec_receive fd = 
  let msg = Bytes.create 64 in
  match recv fd msg 0 64 [] with
  | len -> (Bytes.of_string (String.sub (Bytes.to_string msg) 0 len))

let listen_accept fd =
  listen fd 5;
  print_string "Please give your IP Address and this port number to your opponent: \n";
  print_endline "Your IP Address: ";
  let code = 
    (if env () = Apple then system 
         "ifconfig en0 | grep broadcast | grep -o 'inet\ [0-9]*\\.[0-9]*\\.[0-9]*\\.[0-9]*' | grep -o '[0-9]*\\.[0-9]*\\.[0-9]*\\.[0-9]*'"
     else system "ifconfig eth0 | grep broadcast | grep -o 'inet\ [0-9]*\\.[0-9]*\\.[0-9]*\\.[0-9]*' | grep -o '[0-9]*\\.[0-9]*\\.[0-9]*\\.[0-9]*'")
  in
  print_newline ();
  if code = WEXITED 0 then (print_string ("Port number: \n" ^ string_of_int (find_port fd) ^ "\n");)
  else (print_string "We couldn't find your IP Address. You will have to find it manually.\n");
  print_endline "Waiting for opponent to connect...";
  accept fd

let conn_client fd = 
  print_string "Please enter your opponent's IP Address:\n";
  let ip = read_line () in
  print_string "Please enter the port number to connect to on your opponents machine:\n";
  let port = read_line () in
  let conn_addr = ADDR_INET(inet_addr_of_string ip,int_of_string port) in
  (print_board (new_game ()).pieces; Pervasives.print_newline (); connect fd conn_addr)

let conn_spec fd = 
  print_string "Please enter the IP Address of the host of the game you want to connect to:\n";
  let ip = read_line () in
  print_string "Please enter the port number to connect to:\n";
  let port = read_line () in
  let conn_addr = ADDR_INET(inet_addr_of_string ip,int_of_string port) in
  connect fd conn_addr; print_board (new_game ()).pieces; Pervasives.print_newline ()

let init_spectators fd num =
  let rec fork_list num acc = 
    if num = 0 then acc else
      (pipe ())::(fork_list (num-1) acc)
  in
  let lst = fork_list num [] in
  let fork_accept fd iofd = 
    let conn_fd, conn_sock = accept fd in
    while true do
      let bs = Bytes.create 64 in
      let bytes_read = read (fst iofd) bs 0 64 in
      let sent = send conn_fd bs 0 bytes_read [] in
      if sent < bytes_read then (print_string "The whole command did not send. There is probably a bug")
      else ()
    done
  in
  let rec fork_pipes fd lst =
    match lst with
    | [] -> ()
    | (inp,out)::t ->  match fork () with
      | 0 -> close out; fork_accept fd (inp,out)
      | pid -> close inp; fork_pipes fd t
  in
  fork_pipes fd lst;
  lst