open OUnit2
open Api
open Yojson.Basic.Util

(*******************************************************************************
  Test suite for functions in artist.ml
  *******************************************************************************)
let beatles =
  let json = Yojson.Basic.from_file "test/test_data/beatles.json" in
  let artist = Api.Artist.artist_of_json json in
  artist

let beatles_top_track () : Track.track list =
  let json = Yojson.Basic.from_file "test/test_data/beatles_top_track.json" in
  let top_tracks = json |> to_list |> List.map Track.track_of_json in
  top_tracks

let artist_test =
  [
    ( "Artist Name test" >:: fun _ ->
      assert_equal "The Beatles" (Api.Artist.get_artist_name beatles) );
    ( "Artist of json test" >:: fun _ ->
      assert_equal beatles
        (Api.Artist.artist_of_json
           (Yojson.Basic.from_file "test/test_data/beatles.json")) );
  ]

(*******************************************************************************
  Test suite for functions in track.ml
  *******************************************************************************)
let hotel =
  let json = Yojson.Basic.from_file "test/test_data/hotel.json" in
  let track = Api.Track.track_of_json json in
  track

let track_test =
  [
    ( "Track name test" >:: fun _ ->
      assert_equal "Hotel California - 2013 Remaster"
        (Api.Track.get_track_name hotel) );
    ( "Track of json test" >:: fun _ ->
      assert_equal hotel
        (Api.Track.track_of_json
           (Yojson.Basic.from_file "test/test_data/hotel.json")) );
  ]

(*******************************************************************************
  Test suite for functions in album.ml
  *******************************************************************************)
let album_test = []

let suite =
  "test suite for Spoticaml"
  >::: List.flatten [ album_test; artist_test; track_test ]

let _ = run_test_tt_main suite
