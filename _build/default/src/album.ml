type album = {
  artists : Api.Artist.artist list;
  spotify_link : string;
  images : Api.Artist.image list;
  name : string;
  release_date : string;
  total_tracks : int;
}

open Yojson.Basic.Util
let album_of_json json =
  {
    artists = json |> member "artists" |> to_list |> List.map Artist.artist_of_json;
    spotify_link = json |> member "external_urls" |> member "spotify" |> to_string;
    images = json |> member "images" |> to_list |> List.map Artist.image_of_json;
    name = json |> member "name" |> to_string;
    release_date = json |> member "release_date" |> to_string;
    total_tracks = json |> member "total_tracks" |> to_int;
  }