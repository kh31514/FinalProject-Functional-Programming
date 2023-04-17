type album = {
  album_group : string;
  album_type : string;
  artists : Artist.artist list;
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

open Yojson.Basic.Util

let album_of_json json =
  {
    album_group = json |> member "album_group" |> to_string;
    album_type = json |> member "album_type" |> to_string;
    artists =
      json |> member "artists" |> to_list |> List.map Artist.artist_of_json;
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

let get_album_name () =
  let json = Yojson.Basic.from_file "data/artist.json" in
  let album = album_of_json json in
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

let get_album_artists () =
  let json = Yojson.Basic.from_file "data/album.json" in
  let album = album_of_json json in
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
  } -> artists
