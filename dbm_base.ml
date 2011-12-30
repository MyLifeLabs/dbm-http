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

let encode_position pos = Dbm_util.encode_int64 pos
let decode_position s = Dbm_util.decode_int64 s 0

let add_record db oc decode_value json =
  let key = json |> member "key" |> to_string in
  let value = json |> member "value" |> decode_value in
  let pos = Dbm_data.write_value oc value in
  Dbm.replace db key (encode_position pos)

let load dbname db_mode data_format ic =
  let data_fname = Dbm_data.data_filename dbname in
  let flags =
    match db_mode with
        `Create ->
          if List.exists Sys.file_exists [ dbname;
                                           data_fname;
                                           dbname ^ ".dir";
                                           dbname ^ ".pag"; ]
          then
            failwith (sprintf "File %s[.data|.dir|.pag] already exists" dbname)
          else
            [Dbm.Dbm_wronly; Dbm.Dbm_create]

      | `Update ->
          [Dbm.Dbm_wronly]
  in
  let decode_value =
    match data_format with
        `Json -> (fun json -> Yojson.Basic.to_string json)
      | `Hex -> (function `String s -> Dbm_util.hex_decode s
                   | _ -> failwith "Value is not a hex-encoded string")
      | `Raw -> (function `String s -> s
                   | _ -> failwith "Value is not a string")
  in
  let db = Dbm.opendbm dbname flags 0o666 in
  let oc = Dbm_data.open_for_writing data_fname in
  let finally () =
    (try Dbm.close db with _ -> ());
    close_out_noerr oc
  in
  try
    let strm = Yojson.Basic.stream_from_channel ic in
    Stream.iter (add_record db oc decode_value) strm;
    finally ()
  with e ->
    finally ();
    raise e

let dump dbname data_format oc =
  let encode_key_value =
    match data_format with
        `Json -> (
          fun k v -> 
            Yojson.Basic.to_string
              (`Assoc [ "key", `String k;
                        "value", Yojson.Basic.from_string v ])
          )
      | `Hex -> (
          fun k v ->
            Yojson.Basic.to_string
              (`Assoc [ "key", `String k;
                        "value", `String (Dbm_util.hex_encode v) ])
          )
      | `Raw -> (
          fun k v ->
            Yojson.Basic.to_string
              (`Assoc [ "key", `String k;
                        "value", `String v ])
        )
  in
  let db = Dbm.opendbm dbname [Dbm.Dbm_rdonly] 0o666 in
  let ic = Dbm_data.open_for_reading (Dbm_data.data_filename dbname) in
  let finally () = try Dbm.close db with _ -> () in
  let keygen = ref (fun db -> assert false) in
  keygen := (fun db -> keygen := Dbm.nextkey; Dbm.firstkey db);
  try
    while true do
      let k =
        try !keygen db
        with Not_found -> raise Exit
      in
      let pos = decode_position (Dbm.find db k) in
      seek_in ic pos;
      let v = Dbm_data.read_value ic in
      output_string oc (encode_key_value k v);
      output_char oc '\n';
    done;
    assert false
  with
      Exit ->
        finally ()
    | e ->
        finally ();
        raise e


let get dbname key =
  with_db_read dbname (
    fun db ->
      try
        let pos = decode_position (Dbm.find db key) in
        let value =
          Dbm_data.with_data_in dbname ~pos
            (fun ic -> Dbm_data.read_value ic)
        in
        Some value
      with Not_found -> None
  )

let set dbname key value =
  with_db_write dbname (
    fun db ->
      let pos =
        Dbm_data.with_data_out dbname
          (fun oc -> Dbm_data.write_value oc value)
      in
      Dbm.replace db key (encode_position pos)
  )

let del dbname key =
  with_db_write dbname (
    fun db ->
      (try Dbm.remove db key
       with Dbm.Dbm_error "dbm_delete" -> ())
  )

let get_formatted format dbname key =
  match get dbname key with
      None -> None
    | Some value ->
        let formatted_value =
          match format with
              `Json -> Yojson.Basic.prettify value ^ "\n"
            | `Raw -> value
            | `Hex -> Dbm_util.hex_encode value ^ "\n"
        in
        Some formatted_value

let set_formatted format dbname key formatted_value =
  let value =
    match format with
        `Json -> Yojson.Basic.compact formatted_value
      | `Raw -> formatted_value
      | `Hex -> Dbm_util.hex_decode formatted_value
  in
  set dbname key value
