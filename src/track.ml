type abbrev_artist = {
  spotify_link : string;
  href : string;
  id : string;
  name : string;
  category : string;
  uri : string;
}

type abbrev_album = {
  album_group : string;
  album_type : string;
  artists : abbrev_artist list;
  available_markets : string list;
  spotify_link : string;
  href : string;
  id : string;
  images : Artist.image list;
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
  (* isrc : string; irsc = International Standard Recording Code *)
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

let abbrev_album_of_json json =
  {
    album_group = json |> member "album_group" |> to_string;
    album_type = json |> member "album_type" |> to_string;
    artists =
      json |> member "artists" |> to_list |> List.map abbrev_artist_of_json;
    available_markets =
      json |> member "available_markets" |> to_list |> List.map to_string;
    spotify_link =
      json |> member "external_urls" |> member "spotify" |> to_string;
    href = json |> member "href" |> to_string;
    id = json |> member "id" |> to_string;
    images = json |> member "images" |> to_list |> List.map Artist.image_of_json;
    name = json |> member "name" |> to_string;
    release_date = json |> member "release_date" |> to_string;
    release_date_precision =
      json |> member "release_date_precision" |> to_string;
    total_tracks = json |> member "total_tracks" |> to_int;
    category = json |> member "type" |> to_string;
    uri = json |> member "uri" |> to_string;
  }

let track_of_json json =
  {
    album = json |> member "album" |> abbrev_album_of_json;
    artists =
      json |> member "artists" |> to_list |> List.map abbrev_artist_of_json;
    disc_number = json |> member "disc_number" |> to_int;
    duration_ms = json |> member "duration_ms" |> to_int;
    explicit = json |> member "explicit" |> to_bool;
    (* isrc = json |> member "external_ids" |> member "isrc" |> to_string; *)
    spotify_link =
      json |> member "external_urls" |> member "spotify" |> to_string;
    href = json |> member "href" |> to_string;
    id = json |> member "id" |> to_string;
    is_local = json |> member "is_local" |> to_bool;
    name = json |> member "name" |> to_string;
    popularity = json |> member "popularity" |> to_int;
    preview_url = json |> member "preview_url" |> to_string;
    track_number = json |> member "track_number" |> to_int;
    category = json |> member "type" |> to_string;
    uri = json |> member "uri" |> to_string;
  }

let get_track_name () =
  let json = Yojson.Basic.from_file "data/track.json" in
  let track = track_of_json json in
  match track with
  | {
   album;
   artists;
   disc_number;
   duration_ms;
   explicit;
   (* isrc; *)
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

let get_track_artist () =
  let json = Yojson.Basic.from_file "data/track.json" in
  let track = track_of_json json in
  match track with
  | {
   album;
   artists;
   disc_number;
   duration_ms;
   explicit;
   (* isrc; *)
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
  } -> artists

let get_abbrev_artist_name (a : abbrev_artist) =
  match a with
  | { spotify_link; href; id; name; category; uri } -> name

let format_artists (artist_list : abbrev_artist list) =
  let names = List.map get_abbrev_artist_name artist_list in
  let rec get_str (name_list : string list) =
    match name_list with
    | [] -> ""
    | [ a1 ] -> a1
    | [ a1; a2 ] -> a1 ^ ", and " ^ a2
    | h :: t -> h ^ ", " ^ get_str t
  in
  get_str names
