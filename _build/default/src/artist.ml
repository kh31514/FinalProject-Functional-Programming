exception UnknownArtist of string

type image = {
  height : int;
  url : string;
  width : int;
}

type artist = {
  spotify_link : string;
  follower_count : int;
  genres : string list;
  href : string;
  id : string;
  images : image list;
  name : string;
  popularity : int;
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

let artist_of_json json =
  if json = `Null then
    raise (UnknownArtist (open_in "data/user_input.txt" |> input_line))
  else
    {
      spotify_link =
        json |> member "external_urls" |> member "spotify" |> to_string;
      follower_count = json |> member "followers" |> member "total" |> to_int;
      genres = json |> member "genres" |> to_list |> List.map to_string;
      href = json |> member "href" |> to_string;
      id = json |> member "id" |> to_string;
      images = json |> member "images" |> to_list |> List.map image_of_json;
      name = json |> member "name" |> to_string;
      popularity = json |> member "popularity" |> to_int;
      category = json |> member "type" |> to_string;
      uri = json |> member "uri" |> to_string;
    }

let get_artist () =
  let json = Yojson.Basic.from_file "data/artist.json" in
  let artist = artist_of_json json in
  artist

let get_artist_name artist =
  match artist with
  | {
   spotify_link;
   follower_count;
   genres;
   href;
   id;
   images;
   name;
   popularity;
   category;
   uri;
  } -> name

let get_top_tracks () : Track.track list =
  let json = Yojson.Basic.from_file "data/top_artist_tracks.json" in
  let top_tracks = json |> to_list |> List.map Track.track_of_json in
  top_tracks

let rec top_track_string track_list ind =
  match track_list with
  | [] -> ""
  | h :: t ->
      "\t" ^ string_of_int ind ^ ". " ^ Track.get_track_name h ^ "\n"
      ^ top_track_string t (ind + 1)

let get_artist_name artist =
  match artist with
  | {
   spotify_link;
   follower_count;
   genres;
   href;
   id;
   images;
   name;
   popularity;
   category;
   uri;
  } -> name

let print_artist_info artist =
  match artist with
  | {
   spotify_link;
   follower_count;
   genres;
   href;
   id;
   images;
   name;
   popularity;
   category;
   uri;
  } ->
      print_endline "Here's what I found:";
      print_endline
        (name ^ " has "
        ^ string_of_int follower_count
        ^ " followers and a popularity ranking of " ^ string_of_int popularity
        ^ ".");
      print_endline ("Here are a few of " ^ name ^ "'s top songs:");
      let top_tracks = get_top_tracks () in
      print_string (top_track_string top_tracks 1);
      print_string ("Want to listen to " ^ name ^ " now? Go to ");
      ANSITerminal.print_string [ ANSITerminal.green ] spotify_link;
      print_string "\n"
