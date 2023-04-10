type image = {
  (* height : int; *)
  height : string;
  url : string;
  (* width : int; *)
  width : string;
}

type spotify_record = {
  spotify : string;
}

(* type followers_record = {
  href : ;
  total : int;
} *)

type artist = {
  (* external_url : spotify_record;  *)
  (* followers : string; *)
  genres : string list;
  href : string;
  id : string;
  (* images : image list; *)
  name : string;
  (* popularity : int; *)
  (* popularity : string; *)
  category : string;
  uri : string;
}

(* source: https://github.com/thierry-martinez/pyml *)
let () = Py.initialize ()

(* How to read a text file
  May or may not need this 
   source: https://itecnote.com/tecnote/how-to-read-in-lines-from-a-text-file-in-ocaml/ *)
let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines ;;

open Yojson.Basic.Util
let from_json json =
  let image_of_json json =
    {
      (* height = json |> member "height" |> to_string |> int_of_string; *)
      height = json |> member "height" |> to_string;
      url = json |> member "url" |> to_string;
      (* width = json |> member "width" |> to_string |> int_of_string; *)
      width = json |> member "width" |> to_string;
    }
  in
  (* let spotify_record_of_json = 
    {
      spotify = json |> member "spotify" |> to_string;
    }
  in *)
  let artist_of_json json =
    {
      (* external_url = {}
      followers =  *)
      genres = json |> member "genres" |> to_list |> List.map to_string;
      href = json |> member "href" |> to_string;
      id = json |> member "id" |> to_string;
      (* images = json |> member "images" |> to_list |> List.map image_of_json; *)
      name = json |> member "name" |> to_string;
      (* popularity = json |> member "popularity" |> to_string |> int_of_string; *)
      (* popularity = json |> member "popularity" |> to_string; *)
      category = json |> member "type" |> to_string;
      uri = json |> member "uri" |> to_string;
    }
  in
  artist_of_json json
let get_name = 
let json = Yojson.Basic.from_file "artist.json" in 
  let artist = from_json json in 
  match artist with
  (* | {genres; href; id; images; name; popularity; category; uri} -> name *)
  | {genres; href; id; name; category; uri} -> name


(* TODO: implement this *)
(** [handle_song song] will print information about [song] (ex. artist, album, duration, genre) *)
let handle_song song = ()

(* TODO: implement this *)
(** [handle_artist artist] will print information about [artist] (ex. top 5 songs/albums, age, number of songs produced, popularity) *)
let handle_artist artist = (
  (* source: https://ocaml.org/docs/file-manipulation#example *)
  let oc = open_out "artist.txt" in
  (* create or truncate file, return channel *)
  Printf.fprintf oc "%s\n" artist;
  (* write something *)
  close_out oc;

(* DON'T indent this, will throw a weird error *)
let _ = Py.Run.eval ~start:Py.File ("
from authorization import *
handle_artist()") in let _ = print_endline get_name in ())

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