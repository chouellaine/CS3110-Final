open Unix
open State
open Command

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

