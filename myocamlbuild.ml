[@@@ocamlformat "disable"]

open Ocamlbuild_plugin

let ppoll_discover () =
  dep [ "ppoll_discover" ] [ "lib/miou_poll.h" ];
  rule "has_ppoll.native -> miou_poll.h"
    ~deps:[ "lib/conf/has_ppoll.native"; "lib/conf/has_ppoll.c" ]
    ~prod:"lib/miou_poll.h"
  begin fun env build ->
    let test = env "lib/conf/has_ppoll.native" in
    let file = env "lib/conf/has_ppoll.c" in
    let out = env "lib/miou_poll.h" in
    Cmd (S [A test; A file; Sh ">"; Px out])
  end

let generate () =
  let open Ocamlbuild_pack in
  pflag [] "objs_include" (fun p -> S [A "-I"; A p]);
  rule "%.objs -> %.native"
    ~deps:["%.objs"; "lib/conf/cc.byte"]
    ~prod:"%.native"
  begin fun env build ->
    let cnative = env "%.objs" in
    let srcs =
      let dirname = Pathname.dirname cnative in
      let srcs = string_list_of_file cnative in
      List.map (fun filename -> [Pathname.concat dirname filename]) srcs in
    let srcs = List.map Outcome.good (build srcs) in
    let srcs = S (List.map (fun x -> P x) srcs) in
    let out = env "%.native" in
    let compiler = env "lib/conf/cc.byte" in
    Cmd (S [ A compiler; T (tags_of_pathname out); srcs; A "-o"; Px out ])
  end

let miou_poll_config_generate () =
  dep [ "poll_generate" ] [ "lib/miou_poll_config.ml" ];
  rule "generate.native -> miou_poll_config.ml"
    ~dep:"lib/conf/generate.native"
    ~prod:"lib/miou_poll_config.ml"
  begin fun env build ->
    let generate = env "lib/conf/generate.native" in
    let out = env "lib/miou_poll_config.ml" in
    Cmd (S [ A generate; Sh ">"; Px out])
  end

let () =
  dispatch begin function
    | After_rules ->
        generate ();
        ppoll_discover ();
        miou_poll_config_generate ();
        (* libmiou_poll.clib *)
        flag_and_dep ["link"; "ocaml"; "link_miou_poll"] (P "lib/libmiou_poll.a");
        flag ["c"; "use_miou_poll"; "ocamlmklib"] (S[A"-lmiou_poll"]);
        flag ["ocaml"; "use_miou_poll"; "link"; "library"; "byte"] (S[A"-dllib"; A"-lmiou_poll"]);
        flag ["ocaml"; "use_miou_poll"; "link"; "library"; "native"] (S[A"-cclib"; A"-lmiou_poll"])
    | _ -> ()
  end
