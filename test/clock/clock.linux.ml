external get_time : unit -> (int64[@unboxed])
  = "clock_linux_get_time_bytecode" "clock_linux_get_time_native"
  [@@noalloc]

let now () =
  let value = get_time () in
  Int64.to_float value /. 1000000000.
