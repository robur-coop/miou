open Sync

let test01 =
  let description = {text|Test about Miou.Trigger|text} in
  Test.test ~title:"test01" ~description @@ fun () ->
  let signaled_total = ref 0
  and won_total = ref 0
  and lost_total = ref 0
  and raised_total = ref 0 in
  let () =
    Atomic.trace @@ fun () ->
    let trigger = Trigger.create () in
    let () = Atomic.check @@ fun () -> Trigger.is_initial trigger in
    let signaled = ref 0 and won = ref 0 and lost = ref 0 and raised = ref 0 in
    for _ = 1 to 2 do
      Atomic.spawn @@ fun () -> Trigger.signal trigger
    done;
    for _ = 1 to 2 do
      Atomic.spawn @@ fun () ->
      match Trigger.on_signal trigger `X `Y (fun _ `X `Y -> incr signaled) with
      | success -> if success then incr won else incr lost
      | exception Invalid_argument _ -> incr raised
    done;
    Atomic.final @@ fun () ->
    Atomic.check @@ fun () ->
    signaled_total := !signaled_total + !signaled;
    won_total := !won_total + !won;
    lost_total := !lost_total + !lost;
    raised_total := !raised_total + !raised;
    Trigger.is_signaled trigger
    && !won <= 1
    && !won = !signaled
    && !lost <= 2
    && !raised <= !won
    && !raised = Bool.to_int (!lost = 0)
    && !won + !lost + !raised = 2
  in
  [ signaled_total; won_total; lost_total; raised_total ]
  |> List.iter @@ fun total -> Test.check (!total > 0)

let test02 =
  let description = {text|Test about Miou.Computation|text} in
  Test.test ~title:"test02" ~description @@ fun () ->
  let attached_total = ref 0 and unattached_total = ref 0 in
  let () =
    Atomic.trace @@ fun () ->
    let computation = Computation.create () in
    let returns = ref 0 and cancels = ref 0 in
    let () =
      Atomic.spawn @@ fun () ->
      if Computation.try_return computation 101 then incr returns
    in
    let () =
      Atomic.spawn @@ fun () ->
      if Computation.try_cancel computation (Exit, Printexc.get_callstack 1)
      then incr cancels
    in
    let triggers = Array.init 2 @@ fun _ -> Trigger.create () in
    let attached = ref 0 and unattached = ref 0 in
    let () =
      triggers
      |> Array.iter @@ fun trigger ->
         Atomic.spawn @@ fun () ->
         if Computation.try_attach computation trigger then incr attached
         else incr unattached
    in
    Atomic.final @@ fun () ->
    Atomic.check @@ fun () ->
    attached_total := !attached_total + !attached;
    unattached_total := !unattached_total + !unattached;
    begin match Computation.peek computation with
    | Some (Ok 101) when !returns = 1 && !cancels = 0 -> true
    | Some (Error (Exit, _)) when !returns = 0 && !cancels = 1 -> true
    | _ -> false
    end
    && !attached + !unattached = Array.length triggers
    && !attached
       = Array.fold_left
           (fun acc trigger -> acc + Bool.to_int (Trigger.is_signaled trigger))
           0 triggers
  in
  [ attached_total; unattached_total ]
  |> List.iter @@ fun total -> Test.check (!total > 0)

let test03 =
  let description = {text|Test about deletion of triggers.|text} in
  Test.test ~title:"test03" ~description @@ fun () ->
  Atomic.trace @@ fun () ->
  let computation = Computation.create () in
  let triggers = Array.init 4 @@ fun _ -> Trigger.create () in
  let () =
    triggers
    |> Array.iter @@ fun trigger ->
       Atomic.spawn @@ fun () ->
       Atomic.check (fun () -> Computation.try_attach computation trigger);
       Computation.detach computation trigger
  in
  Atomic.final @@ fun () ->
  Atomic.check @@ fun () ->
  Array.for_all Trigger.is_signaled triggers
  &&
  match Atomic.get computation with
  | Cancelled _ | Returned _ -> false
  | Continue { balance; triggers } ->
      balance <= 0
      && List.length triggers <= 2
      &&
      let trigger = Trigger.create () in
      Computation.try_attach computation trigger
      && begin match Atomic.get computation with
      | Cancelled _ | Returned _ -> false
      | Continue { balance; triggers } -> balance = 1 && triggers = [ trigger ]
      end

let () =
  let tests = [ test01; test02; test03 ] in
  let ({ Test.directory } as runner) =
    Test.runner (Filename.concat (Sys.getcwd ()) "_tests")
  in
  let run idx test =
    let idx = succ idx in
    Format.printf "test%02d: %!" idx;
    Test.run runner test;
    Format.printf "ok\n%!"
  in
  Format.printf "Run tests into %s\n%!" directory;
  List.iteri run tests
