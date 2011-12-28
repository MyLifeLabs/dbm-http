type log_level = [ `Err | `Info ]

let string_of_level = function
    `Err -> "err"
  | `Info -> "info"

let log level msg =
  Printf.eprintf "[%s] [%s] %s\n%!"
    (Netdate.mk_mail_date (Unix.gettimeofday ()))
    (string_of_level level)
    msg

let logf level msgf =
  Printf.kprintf (log level) msgf
