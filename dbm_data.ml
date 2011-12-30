(*
  Data file where the values are actually stored (appended).
*)

let data_filename dbname =
  dbname ^ ".data"

let encode_length = Dbm_util.encode_int64
let decode_length = Dbm_util.decode_int64

let write_value oc s =
  let pos = pos_out oc in
  output_string oc (encode_length (String.length s));
  output_string oc s;
  pos

let read_value ic =
  let lens = String.create 8 in
  really_input ic lens 0 8;
  let len = decode_length lens 0 in
  let s = String.create len in
  really_input ic s 0 len;
  s

let open_for_writing fname =
  let oc = 
    open_out_gen
      [ Open_wronly; Open_creat; Open_binary ] 0o666 fname
  in
  let len = out_channel_length oc in
  seek_out oc len;
  oc

let open_for_reading fname =
  open_in fname

let with_file_out fname f =
  let oc = open_for_writing fname in
  try
    let result = f oc in
    close_out oc;
    result
  with e ->
    close_out_noerr oc;
    raise e

let with_file_in fname ?(pos = 0) f =
  let ic = open_for_reading fname in
  try
    seek_in ic pos;
    let result = f ic in
    close_in ic;
    result
  with e ->
    close_in_noerr ic;
    raise e

let with_data_out dbname f =
  with_file_out (data_filename dbname) f

let with_data_in dbname ?pos f =
  with_file_in (data_filename dbname) ?pos f
