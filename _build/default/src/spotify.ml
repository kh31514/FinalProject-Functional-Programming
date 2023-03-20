let handle_song song = ()

let handle_artist artist = ()

let handle_album album = ()



let rec parse text = 
  print_endline ("Is " ^ text ^ " a song, artist, or ablum?");
  match read_line () with
  | exception End_of_file -> ()
  | "song" | "Song" -> handle_song text
  | "artist" | "Artist" -> handle_artist text
  | "album" | "Album" -> handle_album text
  | weird_input -> print_endline ("Can't deciper " ^ weird_input ^ "."); parse text

(** [main ()] prompts the user for a command, then executes the given command or displays a helpful error message *)
let main () = 
  print_endline "Welcome to SpotiCaml. Start by entering a song, artist, or ablum.";
  match read_line () with
  | exception End_of_file -> ()
  | text -> parse text

(* Execute the game engine. Taken from A2 *)
let () = main ()