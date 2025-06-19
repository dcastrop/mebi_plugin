type output_dir_kind =
  | Default of unit
  | Exact of string

type filename_kind =
  | Auto of unit
  | Just of string

type filetype_kind = JSON of unit

val run
  :  output_dir_kind
  -> string option * filename_kind
  -> filetype_kind
  -> Vernac.result_kind
  -> string
