external init_time : unit -> unit = "clock_windows_init"

external get_time : unit -> (int64[@unboxed])
  = "undefined" "clock_windows_get_time"
  [@@noalloc]

let () = init_time ()

let now () =
  let value = get_time () in
  Int64.to_float value /. 1000000000.
