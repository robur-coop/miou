module Exn_bt = Picos.Exn_bt
open Picos_structured
open Picos_sync

(** Helper to check that computation is restored *)
let check join_after scope =
  let open Picos in
  let fiber = Fiber.current () in
  let before = Fiber.get_computation fiber in
  let finally () =
    let after = Fiber.get_computation fiber in
    assert (before == after)
  in
  Fun.protect ~finally @@ fun () ->
  join_after @@ fun bundle ->
  let during = Fiber.get_computation fiber in
  assert (before != during);
  scope bundle

let test_fork_after_terminate () =
  Miou.run @@ fun () ->
  check Bundle.join_after @@ fun bundle ->
  Bundle.terminate bundle;
  match Bundle.fork bundle (fun () -> Printf.printf "Hello!\n%!") with
  | () -> assert false
  | exception Control.Terminate -> ()

let test_fork_after_escape () =
  Miou.run @@ fun () ->
  let escape = ref (Obj.magic ()) in
  begin
    check Bundle.join_after @@ fun bundle -> escape := bundle
  end;
  match Bundle.fork !escape (fun () -> Printf.printf "Hello!\n%!") with
  | () -> assert false
  | exception Invalid_argument _ -> ()

let test_exception_in_child_terminates () =
  match
    Miou.run ~domains:2 @@ fun () ->
    let mutex = Mutex.create () in
    let condition = Condition.create () in
    let blocked = ref false in
    check Bundle.join_after @@ fun bundle ->
    begin
      Bundle.fork bundle @@ fun () ->
      begin
        Mutex.protect mutex @@ fun () ->
        while not !blocked do
          Condition.wait condition mutex
        done
      end;
      raise Exit
    end;
    begin
      Mutex.protect mutex @@ fun () ->
      blocked := true;
      Condition.signal condition;
      while true do
        Condition.wait condition mutex
      done
    end
  with
  | () -> assert false
  | exception _ -> ()

let test_block_raises () =
  Miou.run @@ fun () ->
  match Control.protect Control.block with
  | () -> assert false
  | exception Invalid_argument _ -> ()

let test_error_in_promise_terminates () =
  match
    Miou.run ~domains:2 @@ fun () ->
    Bundle.join_after @@ fun bundle ->
    let promise =
      Bundle.fork_as_promise bundle @@ fun () -> failwith "I failed"
    in
    Control.block () |> ignore;
    Promise.terminate promise
  with
  | () -> assert false
  | exception _ -> ()

let () =
  [
    ( "Bundle"
    , [
        Alcotest.test_case "fork after terminate" `Quick
          test_fork_after_terminate
      ; Alcotest.test_case "fork after escape" `Quick test_fork_after_escape
      ; Alcotest.test_case "exception in child terminates" `Quick
          test_exception_in_child_terminates
      ; Alcotest.test_case "block raises when forbidden" `Quick
          test_block_raises
      ; Alcotest.test_case "error in promise terminates" `Quick
          test_error_in_promise_terminates
      ] )
  ]
  |> Alcotest.run "Picos_structured"
