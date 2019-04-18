type os = Apple | Other

val find_port : Unix.file_descr -> int

val echo : string -> unit

val env : unit -> os