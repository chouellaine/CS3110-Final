type os = Apple | Other

val receive : Unix.file_descr -> bytes

val listen_accept : Unix.file_descr -> Unix.file_descr * Unix.sockaddr

val conn : Unix.file_descr -> unit