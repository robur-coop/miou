module Box = struct
  type 'a t = { value: 'a option Atomic.t; lock: Mutex.t * Condition.t }

  let push t value =
    Mutex.lock (fst t.lock);
    let set = Atomic.compare_and_set t.value None (Some value) in
    Condition.broadcast (snd t.lock);
    Mutex.unlock (fst t.lock);
    set

  let take t =
    Mutex.lock (fst t.lock);
    while Option.is_none (Atomic.get t.value) do
      Condition.wait (snd t.lock) (fst t.lock)
    done;
    let value = Option.get (Atomic.get t.value) in
    Mutex.unlock (fst t.lock);
    value

  let make () =
    { value= Atomic.make None; lock= (Mutex.create (), Condition.create ()) }
end


let prgm () =
  let box = Box.make () in
  let p0 =
    Miou.call @@ fun () ->
    Miouu.sleep 1.;
    assert (Box.push box "Hello World!" = true)
  in
  let p1 = Miou.call @@ fun () -> ignore (Box.take box) in
  let p2 = Miou.call @@ fun () -> ignore (Box.take box) in
  Miou.await_all [ p0; p1; p2 ]
  |> List.iter @@ function Error exn -> raise exn | Ok () -> ()

let () = Miouu.run prgm
