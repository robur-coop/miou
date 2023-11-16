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

let pp_src ppf = function
  | None -> ()
  | Some src -> Format.fprintf ppf "[%s]" src

let pp_exec_header =
  let x =
    match Array.length Sys.argv with
    | 0 -> Filename.basename Sys.executable_name
    | _ -> Filename.basename Sys.argv.(0)
  in
  let pf = Format.fprintf in
  let pp_header ppf ((domain : Domain.id), level, src, header) =
    if level = App then
      match header with
      | None -> ()
      | Some header -> pf ppf "[%d][%s]%a " (domain :> int) header pp_src src
    else
      match header with
      | None ->
          pf ppf "%s: [%d][%a]%a " x (domain :> int) pp_level level pp_src src
      | Some header ->
          pf ppf "%s: [%d][%s]%a " x (domain :> int) header pp_src src
  in
  pp_header

let make_formatter oc =
  Format.make_formatter (output_substring oc) (fun () -> flush oc)

(* NOTE(dinosaure): it's mandatory to make our own formatter to avoid data-race
   condition if the user uses something like [logs]. *)
let stdout = make_formatter stdout
let stderr = make_formatter stderr

let report src level ~over k msgf =
  let k _ = over (); k () in
  msgf @@ fun ?(domain = Domain.self ()) ?header fmt ->
  let ppf = if level = App then stdout else stderr in
  Format.kfprintf k ppf
    ("%a@[" ^^ fmt ^^ "@]@.")
    pp_exec_header
    (domain, level, src, header)

let mutex_logs = Mutex.create ()

let kmsg : type a b. (unit -> b) -> ?src:string -> level -> (a, b) msgf -> b =
 fun k ?src level msgf ->
  match Sys.getenv_opt "MIOU_DEBUG" with
  | Some _ ->
      let over () = Mutex.unlock mutex_logs in
      Mutex.lock mutex_logs;
      report src level ~over k msgf
  | _ -> k ()

let msg level msgf = kmsg (Fun.const ()) level msgf
let debug msgf = msg Debug msgf
let err msgf = msg Error msgf
let warn msgf = msg Warning msgf

module Make (X : sig
  val src : string
end) =
struct
  let msg level msgf = kmsg ~src:X.src (Fun.const ()) level msgf
  let debug msgf = msg Debug msgf
  let err msgf = msg Error msgf
  let warn msgf = msg Warning msgf
end
