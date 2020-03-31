open Cohttp
open Cohttp_lwt_unix
(*open Lwt*)
open Printf

module M = Mariadb.Blocking

let tmpl = Mustache.of_string "Hello {{name}}!\n"
let json = `O ["name", `String "Ahmet Emre"]

let or_die = function
| Ok x -> x
| Error (i, e) -> failwith @@ sprintf "(%d) %s" i e

let rec print_rows xs = xs
                    |> M.Res.fetch (module M.Row.Array)
                    |> (function
                       | Ok (Some rs) -> Array.iter (fun f -> match M.Field.value f with
                                                    | `String s -> Printf.printf "%s\n" s
                                                    | `Int i -> Printf.printf "%d\n" i
                                                    | _ -> Printf.printf "other\n"
                                                    ) rs;
                                         print_rows xs
                       | _ -> print_endline ""
                       )

let mariadb_main () =
  print_endline @@ Mustache.render tmpl json;
  let mariadb =
    M.connect ~user:"sahip" ~pass:"123" ~db:"test" () |> or_die in
    let query = "SELECT * FROM students" in
  let stmt = M.prepare mariadb query |> or_die in
  let res = M.Stmt.execute stmt [| |] |> or_die in
    printf "number of rows: %d\n%!" (M.Res.num_rows res);
    print_rows res;
    M.Stmt.close stmt |> or_die;
    M.close mariadb;
    M.library_end ()

let read_file path :string =
  let rec aux f acc =
    try
      aux f (input_line f :: acc)
    with
    | End_of_file -> List.rev acc |> String.concat "\n"
  in
  let f = open_in path in
  let resp = aux f [] in
    close_in_noerr f;
    resp

let load_partial s = read_file ("view/" ^ s ^ ".tpl.html")
                     |> Mustache.of_string
							|> Option.some

let view_generic path = read_file path
                        |> Mustache.of_string
								|> Mustache.render ~partials:load_partial

let view_homepage = view_generic "view/homepage.tpl.html"
                      (`O [])

let view_404 m p = view_generic "view/404.tpl.html"
                     (`O ["method", `String m
                         ;"url", `String p])

let view_profile u = view_generic "view/profile.tpl.html"
                       (`O ["userid", `String u])

let view_profile_edit u = view_generic "view/profile-edit.tpl.html"
                            (`O ["userid", `String u
                                ;"title", `String u
                                ;"assets", `A []
										  ])

let view_exn e s = `Bad_request,
                   (view_generic "view/400.tpl.html"
                      (`O ["error", `String e
                          ;"stack", `String s]))

let server =
  let router _headers _body (x:Code.meth * string list) :Code.status_code*string =
    try match x with
    | `GET, [""; ""]           -> `OK, view_homepage
    | `GET, [""; user]         -> `OK, view_profile user
    | `GET, [""; user; "edit"] -> `OK, view_profile_edit user
    | m, p                     -> `Not_found, view_404 (Code.string_of_method m) (String.concat "/" p)
    with
    | e -> view_exn (Printexc.to_string e) (Printexc.get_backtrace ())
  in
  let callback _conn req body =
    let uri = req |> Request.uri in
    let paths = uri |> Uri.path |> String.split_on_char '/' in
    let meth = req |> Request.meth in
    let headers = req |> Request.headers |> Header.to_string in
    let () = printf "%s %s\n" (Code.string_of_method meth) (Uri.path uri) in
      router headers body (meth, paths)
      |> fun (status, body) -> Server.respond_string ~status ~body ()
  in
    Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

(* let () = mariadb_main () *)
let () = ignore (Lwt_main.run server)
