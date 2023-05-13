open OUnit2
open Api
open Yojson.Basic.Util

exception UnknownSong of string

(*******************************************************************************
  Test suite for functions in artist.ml
  *******************************************************************************)
let beatles =
  let json = Yojson.Basic.from_file "test/test_data/beatles.json" in
  let artist = Api.Artist.artist_of_json json in
  artist

let beatles_top_track =
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
    ( "Artist top track list" >:: fun _ ->
      assert_equal
        "\t1. Here Comes The Sun - Remastered 2009\n\
         \t2. Come Together - Remastered 2009\n\
         \t3. Let It Be - Remastered 2009\n\
         \t4. Yesterday - Remastered 2009\n\
         \t5. Twist And Shout - Remastered 2009\n"
        (Api.Artist.top_track_string beatles_top_track 1) );
  ]

(*******************************************************************************
  Test suite for functions in track.ml
  *******************************************************************************)
let hotel =
  let json = Yojson.Basic.from_file "test/test_data/hotel.json" in
  let track = Api.Track.track_of_json json in
  track

let wmggw =
  let json = Yojson.Basic.from_file "test/test_data/WMGGW_tom.json" in
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
    ( "Track artist name test" >:: fun _ ->
      assert_equal "Eagles" (Api.Track.get_track_artist hotel) );
    ( "Obscure name test" >:: fun _ ->
      assert_equal "While My Guitar Gently Weeps"
        (Api.Track.get_track_name wmggw) );
    ( "Obscure track of json test" >:: fun _ ->
      assert_equal wmggw
        (Api.Track.track_of_json
           (Yojson.Basic.from_file "test/test_data/WMGGW_tom.json")) );
    ( "Track artist many name test" >:: fun _ ->
      assert_equal
        "Tom Petty, Jeff Lynne, Steve Winwood, Dhani Harrison and Prince"
        (Api.Track.get_track_artist wmggw) );
  ]

(*******************************************************************************
  Test suite for functions in album.ml
  *******************************************************************************)
let recovery =
  let json = Yojson.Basic.from_file "test/test_data/recovery.json" in
  let album = Api.Album.album_of_json json in
  album

let recovery_list =
  let json = Yojson.Basic.from_file "test/test_data/recovery_tracks.json" in
  let track_list = json |> to_list |> List.map Api.Album.abbrev_track_of_json in
  track_list

let rumors =
  let json = Yojson.Basic.from_file "test/test_data/rumors.json" in
  let album = Api.Album.album_of_json json in
  album

let rumors_list =
  let json = Yojson.Basic.from_file "test/test_data/rumors_tracks.json" in
  let track_list = json |> to_list |> List.map Api.Album.abbrev_track_of_json in
  track_list

let album_test =
  [
    ( "Album name test" >:: fun _ ->
      assert_equal "Recovery" (Api.Album.get_album_name recovery) );
    ( "Album of json test" >:: fun _ ->
      assert_equal recovery
        (Api.Album.album_of_json
           (Yojson.Basic.from_file "test/test_data/recovery.json")) );
    ( "Get album artist test" >:: fun _ ->
      assert_equal "Eminem" (Api.Album.get_album_artists recovery) );
    ( "Track number to name 1 test" >:: fun _ ->
      assert_equal "Cold Wind Blows"
        (Api.Album.track_num_to_name 1 recovery_list) );
    ( "Track number to name last number test" >:: fun _ ->
      assert_equal "Love The Way You Lie"
        (Api.Album.track_num_to_name 15 recovery_list) );
    ( "Track number to name out of bounds test" >:: fun _ ->
      assert_equal "track number not found?"
        (Api.Album.track_num_to_name 17 recovery_list) );
    ( "Track number to name zero test" >:: fun _ ->
      assert_equal "track number not found?"
        (Api.Album.track_num_to_name 0 recovery_list) );
    ( "Track number to name negative test" >:: fun _ ->
      assert_equal "track number not found?"
        (Api.Album.track_num_to_name (-1) recovery_list) );
    ( "Album name test 2" >:: fun _ ->
      assert_equal "Rumours (Super Deluxe)" (Api.Album.get_album_name rumors) );
    ( "Album of json test 2" >:: fun _ ->
      assert_equal rumors
        (Api.Album.album_of_json
           (Yojson.Basic.from_file "test/test_data/rumors.json")) );
    ( "Get album artist 2 test" >:: fun _ ->
      assert_equal "Fleetwood Mac" (Api.Album.get_album_artists rumors) );
    ( "Track number to name 1 test 2" >:: fun _ ->
      assert_equal "Second Hand News - 2004 Remaster"
        (Api.Album.track_num_to_name 1 rumors_list) );
    ( "Track number to name last number test 2" >:: fun _ ->
      assert_equal "Silver Springs - 2004 Remaster"
        (Api.Album.track_num_to_name 12 rumors_list) );
    ( "Track number to name out of bounds test 2" >:: fun _ ->
      assert_equal "track number not found?"
        (Api.Album.track_num_to_name 17 rumors_list) );
    ( "Track number to name zero test 2" >:: fun _ ->
      assert_equal "track number not found?"
        (Api.Album.track_num_to_name 0 rumors_list) );
    ( "Track number to name negative test 2" >:: fun _ ->
      assert_equal "track number not found?"
        (Api.Album.track_num_to_name (-1) rumors_list) );
  ]

let suite =
  "test suite for Spoticaml"
  >::: List.flatten [ album_test; artist_test; track_test ]

let _ = print_endline (Api.Album.track_num_to_name 15 rumors_list)
let _ = run_test_tt_main suite
