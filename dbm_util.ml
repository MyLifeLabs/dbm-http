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


(* 8-byte, big-endian encoding of ints *)

let encode_int64 n =
  let s = String.create 8 in
  for i = 0 to 7 do
    s.[i] <- Char.chr ((n lsr (8 * (7 - i))) land 0xff)
  done;
  s

let decode_int64 s pos =
  let n = ref 0 in
  for i = 0 to 7 do
    n := (!n lsl 8) lor (Char.code s.[pos+i])
  done;
  !n

let test_int64_encoding () =
  let test x = assert (decode_int64 (encode_int64 x) 0 = x) in
  List.iter test [ 0; 1; 340874570987; max_int ]
