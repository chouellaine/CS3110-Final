type os = Apple | Other

val sto64b : string -> bytes

val receive : Unix.file_descr -> bytes

val spec_receive : Unix.file_descr -> bytes

val listen_same : Unix.file_descr -> unit

val listen_accept : Unix.file_descr -> Unix.file_descr * Unix.sockaddr

val conn_client : Unix.file_descr -> unit

val conn_spec : Unix.file_descr -> unit

val init_spectators : Unix.file_descr -> int -> (Unix.file_descr * Unix.file_descr) list

val write_children : (Unix.file_descr * Unix.file_descr) list -> string -> unit