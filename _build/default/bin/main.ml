(* source: https://github.com/thierry-martinez/pyml *)
let () = Py.initialize ()

(* source: https://ocaml.org/docs/file-manipulation#example *)
let upload_input input = 
  let oc = open_out "data/user_input.txt" in
  Printf.fprintf oc "%s\n" input;
  close_out oc; ()

(* TODO: implement this *)
(** [handle_song song] will print information about [song] (ex. artist, album, duration, genre) *)
let handle_song song = ()

(* TODO: implement this *)
(** [handle_artist artist] will print information about [artist] (ex. top 5 songs/albums, age, number of songs produced, popularity) *)
let handle_artist artist = (
  upload_input artist;
(* DON'T indent this, will throw a weird error *)
Py.Run.eval ~start:Py.File ("
from authorization import *
handle_artist()"); 
print_endline (Api.Artist.get_artist_name ());
)

(* TODO: implement this *)
(** [handle_album album] will print information about [album] (ex. songs in album, artist, duration, genre) *)
let handle_album album = ()

(** [parse text] determines whether a user inputs a song, artist, or album and calls the approprate, associaetd function.
    The function will be re-called if given extraneous input *)
let rec parse text = 
  print_endline ("Is " ^ text ^ " a song, artist, or album?");
  match read_line () with
  | exception End_of_file -> ()
  | "song" -> handle_song text
  | "artist" -> handle_artist text
  | "album" -> handle_album text
  | weird_input -> print_endline ("Can't deciper " ^ weird_input ^ "."); parse text

(** [main ()] prompts the user for a command, then executes the given command or displays a helpful error message *)
let main () = 
  print_endline "Welcome to SpotiCaml. Start by entering a song, artist, or album.";
  match read_line () with
  | exception End_of_file -> ()
  | text -> parse text

(* Execute the game engine. Taken from A2 *)
let () = main ()