open Printf

open Yojson.Basic.Util

type db_mode = [ `Create | `Update ]
type data_format = [ `Json | `Hex | `Raw ]

let with_db mode dbname f =
  let flags =
    match mode with
        `Read -> [Dbm.Dbm_rdonly]
      | `Write -> [Dbm.Dbm_wronly]
  in
  let db = Dbm.opendbm dbname flags 0o666 in
  try
    let result = f db in
    Dbm.close db;
    result
  with e ->
    Dbm.close db;
    raise e

let with_db_read = with_db `Read
let with_db_write = with_db `Write

let hexdigit n =
  if n < 10 then Char.chr (48 + n)
  else Char.chr (87 + n)

let unhexdigit c =
  match c with
      '0'..'9' -> Char.code c - 48
    | 'a'..'f' -> Char.code c - 87
    | 'A'..'F' -> Char.code c - 55
    | _ -> failwith "Invalid hex sequence"

let hex_encode s =
  let len = String.length s in
  let sx = String.create (2 * len) in
  for i = 0 to len - 1 do
    let c = Char.code s.[i] in
    let j = 2 * i in
    sx.[j] <- hexdigit (c lsr 4);
    sx.[j+1] <- hexdigit (c land 0b1111);
  done;
  sx

let hex_decode sx =
  let lenx = String.length sx in
  if lenx mod 2 <> 0 then
    failwith "Invalid hex sequence";
  let len = lenx / 2 in
  let s = String.create len in
  for i = 0 to len - 1 do
    let j = 2 * i in
    s.[i] <- Char.chr ((unhexdigit sx.[j] lsl 4) lor (unhexdigit sx.[j+1]))
  done;
  s

let test_hex () =
  let s = String.create 256 in
  for i = 0 to 255 do s.[i] <- Char.chr i done;
  let sx = hex_encode s in
  let s' = hex_decode sx in
  assert (s = s');
  assert (hex_decode (String.uppercase sx) = s)

let add_record db decode_value json =
  let key = json |> member "key" |> to_string in
  let value = json |> member "value" |> decode_value in
  Dbm.replace db key value

let load dbname db_mode data_format ic =
  let flags =
    match db_mode with
        `Create ->
          if List.exists Sys.file_exists [ dbname;
                                           dbname ^ ".pag";
                                           dbname ^ ".dir" ] then
            failwith (sprintf "File %s[.dir|.pag] already exists" dbname)
          else
            [Dbm.Dbm_wronly; Dbm.Dbm_create]

      | `Update ->
          [Dbm.Dbm_wronly]
  in
  let decode_value =
    match data_format with
        `Json -> (fun json -> Yojson.Basic.to_string json)
      | `Hex -> (function `String s -> hex_decode s
                   | _ -> failwith "Value is not a hex-encoded string")
      | `Raw -> (function `String s -> s
                   | _ -> failwith "Value is not a string")
  in
  let db = Dbm.opendbm dbname flags 0o666 in
  let finally () = try Dbm.close db with _ -> () in
  try
    let strm = Yojson.Basic.stream_from_channel ic in
    Stream.iter (add_record db decode_value) strm;
    finally ()
  with e ->
    finally ();
    raise e

let get dbname key =
  with_db_read dbname (
    fun db ->
      try Some (Dbm.find db key)
      with Not_found -> None
  )

let set dbname key value =
  with_db_write dbname (
    fun db ->
      Dbm.replace db key value
  )
