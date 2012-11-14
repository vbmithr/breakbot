open Ocamlbuild_plugin

let () = dispatch begin function
  | After_rules -> flag ["ocaml"; "compile";] & S[A"-ppopt"; A"-lwt-debug"]
  | _ -> ()
end
