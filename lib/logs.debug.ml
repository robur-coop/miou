type level = App | Error | Warning | Info | Debug

type ('a, 'b) msgf =
     (   ?domain:Domain.id
      -> ?header:string
      -> ('a, Format.formatter, unit, 'b) format4
      -> 'a)
  -> 'b

let pp_level ppf = function
  | App -> ()
  | Error -> Format.pp_print_string ppf "ERROR"
  | Warning -> Format.pp_print_string ppf "WARNING"
  | Info -> Format.pp_print_string ppf "INFO"
  | Debug -> Format.pp_print_string ppf "DEBUG"

let pp_exec_header =
  let x =
    match Array.length Sys.argv with
    | 0 -> Filename.basename Sys.executable_name
    | _ -> Filename.basename Sys.argv.(0)
  in
  let pf = Format.fprintf in
  let pp_header ppf ((domain : Domain.id), level, header) =
    if level = App then
      match header with
      | None -> ()
      | Some header -> pf ppf "[%d][%s] " (domain :> int) header
    else
      match header with
      | None -> pf ppf "%s: [%d][%a] " x (domain :> int) pp_level level
      | Some header -> pf ppf "%s: [%d][%s] " x (domain :> int) header
  in
  pp_header

let report level ~over k msgf =
  let k _ = over (); k () in
  msgf @@ fun ?(domain = Domain.self ()) ?header fmt ->
  let ppf =
    if level = App then Format.std_formatter else Format.err_formatter
  in
  Format.kfprintf k ppf
    ("%a@[" ^^ fmt ^^ "@]@.")
    pp_exec_header (domain, level, header)

let mutex_logs = Mutex.create ()

let kmsg : type a b. (unit -> b) -> level -> (a, b) msgf -> b =
 fun k level msgf ->
  match Sys.getenv_opt "MIOU_DEBUG" with
  | Some _ ->
      let over () = Mutex.unlock mutex_logs in
      Mutex.lock mutex_logs; report level ~over k msgf
  | _ -> k ()

let msg level msgf = kmsg (Fun.const ()) level msgf
let debug msgf = msg Debug msgf
let err msgf = msg Error msgf
let warn msgf = msg Warning msgf
