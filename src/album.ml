exception UnknownAlbum of string

type album = {
  album_group : string;
  album_type : string;
  artists : Track.abbrev_artist list;
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

type abbrev_track = {
  name : string;
  track_number : int;
}

open Yojson.Basic.Util

let album_of_json json =
  if json = `Null then
    raise (UnknownAlbum (open_in "data/user_input.txt" |> input_line))
  else
    {
      album_group = json |> member "album_group" |> to_string;
      album_type = json |> member "album_type" |> to_string;
      artists =
        json |> member "artists" |> to_list
        |> List.map Track.abbrev_artist_of_json;
      available_markets =
        json |> member "available_markets" |> to_list |> List.map to_string;
      spotify_link =
        json |> member "external_urls" |> member "spotify" |> to_string;
      href = json |> member "href" |> to_string;
      id = json |> member "id" |> to_string;
      images =
        json |> member "images" |> to_list |> List.map Artist.image_of_json;
      name = json |> member "name" |> to_string;
      release_date = json |> member "release_date" |> to_string;
      release_date_precision =
        json |> member "release_date_precision" |> to_string;
      total_tracks = json |> member "total_tracks" |> to_int;
      category = json |> member "type" |> to_string;
      uri = json |> member "uri" |> to_string;
    }

let abbrev_track_of_json json =
  {
    name = json |> member "name" |> to_string;
    track_number = json |> member "track_number" |> to_int;
  }

let get_album () =
  let json = Yojson.Basic.from_file "data/album.json" in
  let album = album_of_json json in
  album

let get_album_name album =
  match album with
  | {
   album_group;
   album_type;
   artists;
   available_markets;
   spotify_link;
   href;
   id;
   images;
   name;
   release_date;
   release_date_precision;
   total_tracks;
   category;
   uri;
  } -> name

let get_album_artists album =
  match album with
  | {
   album_group;
   album_type;
   artists;
   available_markets;
   spotify_link;
   href;
   id;
   images;
   name;
   release_date;
   release_date_precision;
   total_tracks;
   category;
   uri;
  } -> Track.format_artists artists

let get_album_tracks () =
  let json = Yojson.Basic.from_file "data/album_tracks.json" in
  let track_list = json |> to_list |> List.map abbrev_track_of_json in
  track_list

let get_album_track_len (tlist : abbrev_track list) =
  let track_list = tlist in
  List.length track_list

let track_num_to_name n list =
  let track_list = list in
  let rec iterate_tracks list =
    match list with
    | h :: t -> if h.track_number = n then h.name else iterate_tracks t
    | [] -> "track number not found?"
  in
  iterate_tracks track_list

let rec album_track_string track_list ind =
  match track_list with
  | [] -> ""
  | h :: t ->
      "\t" ^ string_of_int ind ^ ". " ^ h.name ^ "\n"
      ^ album_track_string t (ind + 1)

let print_album_info album =
  match album with
  | {
   album_group;
   album_type;
   artists;
   available_markets;
   spotify_link;
   href;
   id;
   images;
   name;
   release_date;
   release_date_precision;
   total_tracks;
   category;
   uri;
  } ->
      print_endline "Here's what I found:";
      print_endline
        ("\t" ^ name ^ " was produced by "
        ^ Track.format_artists artists
        ^ " in "
        ^ String.sub release_date 0 4
        ^ ".");
      if total_tracks > 15 then
        print_endline
          ("\t" ^ name ^ " has a total of " ^ string_of_int total_tracks
         ^ " tracks, some of which include ")
      else print_endline ("\t" ^ name ^ " includes the following songs:");
      print_string (album_track_string (get_album_tracks ()) 1);
      print_string "Want to listen now? Go to ";
      ANSITerminal.print_string [ ANSITerminal.green ] spotify_link;
      print_string "\n"
