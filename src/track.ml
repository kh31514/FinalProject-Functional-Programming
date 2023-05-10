exception UnknownSong of string

type image = {
  height : int;
  url : string;
  width : int;
}

(* type abbrev_artist represents an abbreviated version of type artist from
   Artist.ml. Unlike type artist, type abbrev_artist does not include the
   following attributes: follower_count, genres, images, and popularity *)
type abbrev_artist = {
  spotify_link : string;
  href : string;
  id : string;
  name : string;
  category : string;
  uri : string;
}

(* type abbrev_album is the same as type album from Album.ml except,
   abbrev_album utilizes the type abbrev_artist (see above) instead of artist *)
type abbrev_album = {
  album_group : string;
  album_type : string;
  artists : abbrev_artist list;
  available_markets : string list;
  spotify_link : string;
  href : string;
  id : string;
  images : image list;
  name : string;
  release_date : string;
  release_date_precision : string;
  total_tracks : int;
  category : string;
  uri : string;
}

type track = {
  album : abbrev_album;
  artists : abbrev_artist list;
  disc_number : int;
  duration_ms : int;
  explicit : bool;
  isrc : string; (* isrc = International Standard Recording Code *)
  spotify_link : string;
  href : string;
  id : string;
  is_local : bool;
  name : string;
  popularity : int;
  preview_url : string;
  track_number : int;
  category : string;
  uri : string;
}

open Yojson.Basic.Util

let image_of_json json =
  {
    height = json |> member "height" |> to_int;
    url = json |> member "url" |> to_string;
    width = json |> member "width" |> to_int;
  }

(* [abbrev_artist_of_json j] is the abbreviated artist that [j] represents.
   Requires: [j] is a valid JSON abbreviated artist representation. *)
let abbrev_artist_of_json json =
  {
    spotify_link =
      json |> member "external_urls" |> member "spotify" |> to_string;
    href = json |> member "href" |> to_string;
    id = json |> member "id" |> to_string;
    name = json |> member "name" |> to_string;
    category = json |> member "type" |> to_string;
    uri = json |> member "uri" |> to_string;
  }

(* [abbrev_album_of_json j] is the abbreviated album that [j] represents.
   Requires: [j] is a valid JSON abbreviated album representation. *)
let abbrev_album_of_json json =
  {
    album_group = json |> member "album_group" |> to_string;
    album_type = json |> member "album_type" |> to_string;
    artists =
      json |> member "artists" |> to_list |> List.map abbrev_artist_of_json;
    available_markets =
      (try json |> member "available_markets" |> to_list |> List.map to_string
       with _ -> []);
    spotify_link =
      json |> member "external_urls" |> member "spotify" |> to_string;
    href = json |> member "href" |> to_string;
    id = json |> member "id" |> to_string;
    images = json |> member "images" |> to_list |> List.map image_of_json;
    name = json |> member "name" |> to_string;
    release_date = json |> member "release_date" |> to_string;
    release_date_precision =
      json |> member "release_date_precision" |> to_string;
    total_tracks = json |> member "total_tracks" |> to_int;
    category = json |> member "type" |> to_string;
    uri = json |> member "uri" |> to_string;
  }

let track_of_json json =
  if json = `Null then
    raise (UnknownSong (open_in "data/user_input.txt" |> input_line))
  else
    {
      album = json |> member "album" |> abbrev_album_of_json;
      artists =
        json |> member "artists" |> to_list |> List.map abbrev_artist_of_json;
      disc_number = json |> member "disc_number" |> to_int;
      duration_ms = json |> member "duration_ms" |> to_int;
      explicit = json |> member "explicit" |> to_bool;
      isrc =
        (try json |> member "external_ids" |> member "isrc" |> to_string
         with _ -> "");
      spotify_link =
        json |> member "external_urls" |> member "spotify" |> to_string;
      href = json |> member "href" |> to_string;
      id = json |> member "id" |> to_string;
      is_local = json |> member "is_local" |> to_bool;
      name = json |> member "name" |> to_string;
      popularity = (try json |> member "popularity" |> to_int with _ -> -1);
      preview_url =
        (try json |> member "preview_url" |> to_string with _ -> "");
      track_number = json |> member "track_number" |> to_int;
      category = json |> member "type" |> to_string;
      uri = json |> member "uri" |> to_string;
    }

let get_track () =
  let json = Yojson.Basic.from_file "data/track.json" in
  let track = track_of_json json in
  track

let get_track_name track =
  match track with
  | {
   album;
   artists;
   disc_number;
   duration_ms;
   explicit;
   isrc;
   spotify_link;
   href;
   id;
   is_local;
   name;
   popularity;
   preview_url;
   track_number;
   category;
   uri;
  } -> name

(** [get_abbrev_artist_name a] returns the name of abbreviated artist [a]. *)
let get_abbrev_artist_name (a : abbrev_artist) =
  match a with
  | { spotify_link; href; id; name; category; uri } -> name

(* [format_artists] translates an abbrev_artist list to a readable string of
   each artist's name. *)
let format_artists (artist_list : abbrev_artist list) =
  let names = List.map get_abbrev_artist_name artist_list in
  let rec get_str (name_list : string list) =
    match name_list with
    | [] -> ""
    | [ a1 ] -> a1
    | [ a1; a2 ] -> a1 ^ " and " ^ a2
    | h :: t -> h ^ ", " ^ get_str t
  in
  get_str names

let get_track_artist track =
  match track with
  | {
   album;
   artists;
   disc_number;
   duration_ms;
   explicit;
   spotify_link;
   href;
   id;
   is_local;
   name;
   popularity;
   preview_url;
   track_number;
   category;
   uri;
  } -> artists |> format_artists

(* [format duration t] translates [t] from milisecods to a readable string of "_
   minutes and _ seconds" *)
let format_duration ms =
  let seconds = ms / 1000 in
  let minutes = seconds / 60 in
  let remaining_seconds = seconds - (minutes * 60) in
  string_of_int minutes ^ " minutes and "
  ^ string_of_int remaining_seconds
  ^ " seconds"

(* [format_explicit [b] translates an explict boolean [b] to the string "is/is
   not explicit" depending on the value of [b] *)
let format_explicit explicit_bool =
  if explicit_bool then "is explicit" else "is not explicit"

let print_track_info track =
  match track with
  | {
   album;
   artists = track_artists;
   disc_number;
   duration_ms;
   explicit;
   spotify_link = track_spotify;
   href;
   id;
   is_local;
   name = track_name;
   popularity;
   preview_url;
   track_number;
   category;
   uri;
  } -> (
      match album with
      | {
       album_group;
       album_type;
       artists = album_artists;
       (* available_markets; *)
       spotify_link;
       href;
       id;
       images;
       name = album_name;
       release_date;
       release_date_precision;
       total_tracks;
       category;
       uri;
      } ->
          print_endline "Here's what I found:";
          print_endline
            ("\t" ^ track_name ^ " was produced by "
            ^ format_artists track_artists
            ^ " in "
            ^ String.sub release_date 0 4
            ^ ".");
          print_endline
            ("\t" ^ "It " ^ format_explicit explicit ^ ", lasts "
            ^ format_duration duration_ms
            ^ ", and has a popularity ranking of " ^ string_of_int popularity
            ^ ".");
          print_endline
            ("\t" ^ "It is track number " ^ string_of_int track_number
           ^ " in the album " ^ album_name ^ ".");
          print_string "Want to listen now? Go to ";
          ANSITerminal.print_string [ ANSITerminal.green ] spotify_link;
          print_string "\n")
