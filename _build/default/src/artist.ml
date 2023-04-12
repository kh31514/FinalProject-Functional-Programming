type image = {
  height : int;
  url : string;
  width : int;
}

type artist = {
  spotify_link : string;
  follower_count : int;
  genres : string list;
  (* href : string; *)
  (* id : string; *)
  images : image list;
  name : string;
  popularity : int;
      (* category : string; *)
      (* uri : string; *)
}

open Yojson.Basic.Util

let image_of_json json =
  {
    height = json |> member "height" |> to_int;
    url = json |> member "url" |> to_string;
    width = json |> member "width" |> to_int;
  }

let artist_of_json json =
  {
    spotify_link =
      json |> member "external_urls" |> member "spotify" |> to_string;
    follower_count = json |> member "followers" |> member "total" |> to_int;
    genres = json |> member "genres" |> to_list |> List.map to_string;
    (* href = json |> member "href" |> to_string; *)
    (* id = json |> member "id" |> to_string; *)
    images = json |> member "images" |> to_list |> List.map image_of_json;
    name = json |> member "name" |> to_string;
    popularity =
      json |> member "popularity" |> to_int
      (* category = json |> member "type" |> to_string; *)
      (* uri = json |> member "uri" |> to_string; *);
  }

(* The [var] parameter isn't used but without it, get_artist_name immediately
   evaluates to a value at runtime (cause of the OBO error earlier). Basically
   the [var] parameter delays the runtime *)
let get_artist_name var =
  let json = Yojson.Basic.from_file "data/artist.json" in
  let artist = artist_of_json json in
  match artist with
  | { spotify_link; follower_count; genres; images; name; popularity } -> name
(* | {spotify_link; follower_count; genres; href; id; images; name; popularity;
   category; uri} -> name *)
