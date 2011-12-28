open Printf
open Cohttpserver
open Dbm_log

type param = {
  db_name : string;
  rw : bool;
  content_type : string;
  port : int;
  logfile : string;
  format : Dbm_base.data_format;
}

let internal_error_page req e =
  let msg = Printexc.to_string e in
  let h = String.sub (Digest.to_hex (Digest.string msg)) 0 8 in
  let resp_body = sprintf "Internal error (ID %s)" h in
  logf `Err "Internal error: %s (ID %s)" msg h;
  let doc = [ `String resp_body ] in
  let status = `Internal_server_error in
  let headers = [ "Content-type", "text/plain" ] in
  Lwt.return (Cohttp.Http_response.init ~body:doc ~headers ~status ())

let success_page ~content_type s =
  let headers = [ "Content-type", content_type ] in
  let body = [ `String s ] in
  Lwt.return (Cohttp.Http_response.init ~body ~headers ~status:`OK ())

let page_not_found () =
  let headers = [ "Content-type", "text/plain" ] in
  let body = [ `String "Not found" ] in
  Lwt.return (Cohttp.Http_response.init ~body ~headers ~status:`Not_found ())

let handle_get_query p key =
  match Dbm_base.get p.db_name key with
      Some value ->
        let body =
          match p.format with
              `Json -> Yojson.Basic.prettify value
            | `Raw -> value
            | `Hex -> Dbm_base.hex_encode value
        in
        success_page ~content_type: p.content_type body
    | None ->
        page_not_found ()

let handle_set_query p key value0 =
  if not p.rw then
    page_not_found ()
  else
    let value =
      match p.format with
          `Json -> Yojson.Basic.compact value0
        | `Raw -> value0
        | `Hex -> Dbm_base.hex_decode value0
    in
    Dbm_base.set p.db_name key value;
    success_page ~content_type: p.content_type "ok"


let dispatch p req path_elem =
  try
    match path_elem with
        [ ""; "get"; key ] -> handle_get_query p key
      | [ ""; "set"; key; value ] -> handle_set_query p key value
      | _ -> page_not_found ()
  with e ->
    internal_error_page req e

let get_referrer req =
  String.concat " " (Cohttp.Http_request.header req ~name:"Referer")

let main_callback p con_id req =
  let path = Cohttp.Http_request.path req in
  let referrer = get_referrer req in
  logf `Info "%s %s [%s]"
    (Cohttp.Http_common.string_of_method (Cohttp.Http_request.meth req))
    path 
    referrer;
  
  let path_elem = Pcre.split ~pat:"/" path in

  Lwt.bind (dispatch p req path_elem) (
    fun resp ->
      Http_daemon.respond_with resp
  )

let redirect_stdout_stderr fname =
  let oc =
    open_out_gen
      [Open_wronly; Open_creat; Open_append; Open_text]
      0o666 fname
  in
  let fd = Unix.descr_of_out_channel oc in
  Unix.dup2 fd Unix.stdout;
  Unix.dup2 fd Unix.stderr

let start p =
  if Unix.fork () <> 0 then
    exit 0
  else (
    eprintf "log: %s\n%!" p.logfile;
    eprintf "pid: %i\n%!" (Unix.getpid ());
    redirect_stdout_stderr p.logfile;
    logf `Info "START";
    logf `Info "Process ID: %i" (Unix.getpid ());
    logf `Info "Current directory: %s" (Sys.getcwd ());
    logf `Info "HTTP port: %i" p.port;
    logf `Info "Database: %s" p.db_name;
    Lwt_main.run ( 
      let spec = {
        Http_daemon.default_spec with
          Http_daemon.callback = main_callback p;
          port = p.port
      } 
      in
      
      Http_daemon.main spec
    )
  )
