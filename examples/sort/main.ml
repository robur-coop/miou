let () = Printexc.record_backtrace true

let merge ~compare arr lo mi hi =
  let tmp = Array.make (hi - lo) arr.(lo) in
  let k = ref 0 and i = ref lo and j = ref mi in
  while !k < hi - lo do
    if !j = hi then (
      tmp.(!k) <- arr.(!i);
      incr i)
    else if !i = mi then (
      tmp.(!k) <- arr.(!j);
      incr j)
    else if compare arr.(!j) arr.(!i) < 0 then (
      tmp.(!k) <- arr.(!j);
      incr j)
    else (
      tmp.(!k) <- arr.(!i);
      incr i);
    incr k
  done;
  Array.blit tmp 0 arr lo (hi - lo)

let rec sort ~compare (arr, lo, hi) =
  match hi - lo with
  | 0 | 1 -> ()
  | _ ->
      let mi = (lo + hi) / 2 in
      ignore (Miou.parallel (sort ~compare) [ (arr, lo, mi); (arr, mi, hi) ]);
      merge ~compare arr lo mi hi

let sort ~compare arr = sort ~compare (arr, 0, Array.length arr)

let () =
  Miou.run @@ fun () ->
  let arr = [| 4; 65; 2; -31; 0; 99; 2; 83; 782; 1 |] in
  sort ~compare:Int.compare arr;
  Fmt.pr "%a\n%!" Fmt.(Dump.array int) arr
