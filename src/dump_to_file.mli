type output_dir_kind =
  | Default of unit
  | Exact of string

type filename_kind =
  | Auto of unit
  | Just of string
  | LTS of string
  | FSM of string

type filetype_kind = JSON of unit
type dumpable_kind = LTS of Lts.lts

val write_to_file
  :  output_dir_kind
  -> filename_kind
  -> filetype_kind
  -> dumpable_kind
  -> string
