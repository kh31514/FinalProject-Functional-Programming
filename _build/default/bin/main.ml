(* source: https://github.com/thierry-martinez/pyml *)
let () = Py.initialize ()

(* source: https://ocaml.org/docs/file-manipulation#example *)
let upload_input input =
  let oc = open_out "data/user_input.txt" in
  Printf.fprintf oc "%s\n" input;
  close_out oc;
  ()

let rec understand_y_n input y_action n_action =
  match input with
  | "y" | "Y" -> y_action ()
  | "n" | "N" -> n_action ()
  | _ -> (
      print_string "Couldn't understand ";
      ANSITerminal.print_string [ ANSITerminal.green ] input;
      print_endline "\nPlease enter \"y\" or \"n\"";
      match read_line () with
      | exception End_of_file -> ()
      | text ->
          understand_y_n text y_action n_action;
          ())

let print_closing () =
  print_endline "Thank you for using SpotiCaml!";
  ()

(** [handle_song song] will print information about [song] (ex. artist, album,
    duration, genre) *)
let rec handle_song song =
  upload_input song;
  let _ =
    Py.Run.eval ~start:Py.File "\nfrom authorization import *\nhandle_track()"
  in
  ();

  print_string "Are you referring to ";
  ANSITerminal.print_string [ ANSITerminal.green ] (Api.Track.get_track_name ());
  print_string " by ";
  ANSITerminal.print_string [ ANSITerminal.green ]
    (Api.Track.get_track_artist () |> Api.Track.format_artists);
  print_string "? (y/n)\n";
  match read_line () with
  | exception End_of_file -> ()
  | text -> (
      let y_action () = Api.Track.print_track_info (Api.Track.get_track ()) in
      understand_y_n text y_action y_action;
      print_endline
        "Would you like to search for a different song, artist, or album? (y/n)";
      match read_line () with
      | exception End_of_file -> ()
      | text' ->
          let y_action () =
            print_endline "Please enter another song, artist, or album.";
            parse ()
          in
          understand_y_n text' y_action print_closing)

and handle_artist artist =
  upload_input artist;
  let _ =
    Py.Run.eval ~start:Py.File "\nfrom authorization import *\nhandle_artist()"
  in
  ();
  ()

and handle_album album =
  upload_input album;
  let _ =
    Py.Run.eval ~start:Py.File "\nfrom authorization import *\nhandle_album()"
  in
  ();
  ()

and parse () =
  match read_line () with
  | exception End_of_file -> ()
  | text -> (
      print_string "Is ";
      ANSITerminal.print_string [ ANSITerminal.green ] text;
      print_string " a song, artist, or album?\n";
      match read_line () with
      | exception End_of_file -> ()
      | "song" -> handle_song text
      | "artist" -> handle_artist text
      | "album" -> handle_album text
      | weird_input ->
          print_string "Can't deciper ";
          ANSITerminal.print_string [ ANSITerminal.green ] weird_input;
          print_string ".\n";
          parse ())

(** [main ()] prompts the user for a command, then executes the given command or
    displays a helpful error message *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\n\nWelcome to SpotiCaml. Start by entering a song, artist, or album.\n";
  parse ()

(* Execute the game engine. Taken from A2 *)
let () = main ()
