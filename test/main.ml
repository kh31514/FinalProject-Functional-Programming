open OUnit2
open Api
open Yojson.Basic.Util

(*Test Plan For our testing we focussed on making the tests around the key
  funcitonality that intesracts with the Json files that were given to us by the
  API. This is to ensure we are accurately able to pull information from the
  files and that the files contain all of the information that we require. We
  also Needed to ensure uniformity across the files we receive from the API so
  each function tested is tested against multiple Json files. Songs have a lot
  of variation when it comes to the type of producer, whether it be a band,
  single artist, or colaboration. This variation spreads across individual
  artists and albums, so we needed to test all of these different types of media
  in the unit test. Black box testing was used in order to create many of the
  tests. In order to do so we took multiple json files created by the spotify
  API and plugged them directly into the functions used in the src file,
  matching them against the what the output should be according to the mli. Much
  of the functionality of this code exists in the bin/Main.ml file. This is an
  executable file and we are therefore unable to test what is in this file This
  file does, however mostly deal with the user interface portion of the code,
  and therefore was better suited to be tested mannually. This was done by
  running the program several times accross several inputs. This was also
  necessary for a few of the functions in the src file. The src and the bin
  files were heavily interwoven, and some functions in the src file could not be
  tested consistently by a test suite. There functions were therefore also
  tested manually We believe the test suite demonstrates the system is able to
  reliably parse information from json files given back from spotify API. As
  this is the main function of the program, to serve as a go-between for the
  used and the API. Users will accurately receive informaiton back from the API
  due to the functions that have been thuroughly tested in this suite*)
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

let twopac =
  let json = Yojson.Basic.from_file "test/test_data/2pac.json" in
  let artist = Api.Artist.artist_of_json json in
  artist

let twopac_tracks =
  let json = Yojson.Basic.from_file "test/test_data/2pac_top_track.json" in
  let top_tracks = json |> to_list |> List.map Track.track_of_json in
  top_tracks

let kesha =
  let json = Yojson.Basic.from_file "test/test_data/kesha.json" in
  let artist = Api.Artist.artist_of_json json in
  artist

let kesha_tracks =
  let json = Yojson.Basic.from_file "test/test_data/kesha_tracks.json" in
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
    ( "Artist Name 2pac test" >:: fun _ ->
      assert_equal "2Pac" (Api.Artist.get_artist_name twopac) );
    ( "Artist of json test 2pac" >:: fun _ ->
      assert_equal twopac
        (Api.Artist.artist_of_json
           (Yojson.Basic.from_file "test/test_data/2pac.json")) );
    ( "Artist top track list 2pac" >:: fun _ ->
      assert_equal
        "\t1. Hit 'Em Up - Single Version\n\
         \t2. California Love - Original Version\n\
         \t3. Ambitionz Az A Ridah\n\
         \t4. All Eyez On Me (ft. Big Syke)\n\
         \t5. Changes\n"
        (Api.Artist.top_track_string twopac_tracks 1) );
    ( "Artist Name kesha test" >:: fun _ ->
      assert_equal "Kesha" (Api.Artist.get_artist_name kesha) );
    ( "Artist of json test kesha" >:: fun _ ->
      assert_equal kesha
        (Api.Artist.artist_of_json
           (Yojson.Basic.from_file "test/test_data/kesha.json")) );
    ( "Artist top track list kesha" >:: fun _ ->
      assert_equal
        "\t1. TiK ToK\n\
         \t2. Die Young\n\
         \t3. Backstabber\n\
         \t4. We R Who We R\n\
         \t5. Praying\n"
        (Api.Artist.top_track_string kesha_tracks 1) );
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

let careless =
  let json = Yojson.Basic.from_file "test/test_data/careless.json" in
  let track = Api.Track.track_of_json json in
  track

let white =
  let json = Yojson.Basic.from_file "test/test_data/white.json" in
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
    ( "Track name careless test" >:: fun _ ->
      assert_equal "Careless Whisper" (Api.Track.get_track_name careless) );
    ( "Track of json test careless" >:: fun _ ->
      assert_equal careless
        (Api.Track.track_of_json
           (Yojson.Basic.from_file "test/test_data/careless.json")) );
    ( "Track artist one singer name test" >:: fun _ ->
      assert_equal "George Michael" (Api.Track.get_track_artist careless) );
    ( "Track name white christmas test" >:: fun _ ->
      assert_equal "White Christmas" (Api.Track.get_track_name white) );
    ( "Track of json test white christmas single" >:: fun _ ->
      assert_equal white
        (Api.Track.track_of_json
           (Yojson.Basic.from_file "test/test_data/white.json")) );
    ( "Track artist many artists name on single test" >:: fun _ ->
      assert_equal
        "Bing Crosby, Ken Darby Singers and John Scott Trotter & His Orchestra"
        (Api.Track.get_track_artist white) );
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

let wtt =
  let json = Yojson.Basic.from_file "test/test_data/WTT.json" in
  let album = Api.Album.album_of_json json in
  album

let wtt_list =
  let json = Yojson.Basic.from_file "test/test_data/WTT_tracks.json" in
  let track_list = json |> to_list |> List.map Api.Album.abbrev_track_of_json in
  track_list

let harder =
  let json = Yojson.Basic.from_file "test/test_data/htc.json" in
  let album = Api.Album.album_of_json json in
  album

let harder_list =
  let json = Yojson.Basic.from_file "test/test_data/htc_tracks.json" in
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
    ( "Abbrev track of json recovery test" >:: fun _ ->
      assert_equal 15 (Api.Album.get_album_track_len recovery_list) );
    ( "Abbrev track of json Rumours test" >:: fun _ ->
      assert_equal 12
        (Api.Album.get_album_track_len rumors_list)
        ~printer:string_of_int );
    ( "Album long name test" >:: fun _ ->
      assert_equal "Watch The Throne (Deluxe)" (Api.Album.get_album_name wtt) );
    ( "Album of json wtt test" >:: fun _ ->
      assert_equal wtt
        (Api.Album.album_of_json
           (Yojson.Basic.from_file "test/test_data/WTT.json")) );
    ( "Get album artist mult artist test" >:: fun _ ->
      assert_equal "JAY-Z and Kanye West" (Api.Album.get_album_artists wtt) );
    ( "Track number to name 1 test" >:: fun _ ->
      assert_equal "No Church In The Wild"
        (Api.Album.track_num_to_name 1 wtt_list) );
    ( "Track number to name last number test" >:: fun _ ->
      assert_equal "Primetime" (Api.Album.track_num_to_name 15 wtt_list) );
    ( "Track number to name strange name test" >:: fun _ ->
      assert_equal "H•A•M" (Api.Album.track_num_to_name 14 wtt_list) );
    ( "Album name test harder" >:: fun _ ->
      assert_equal "The Harder They Come (Original Motion Picture Soundtrack)"
        (Api.Album.get_album_name harder) );
    ( "Album of json test" >:: fun _ ->
      assert_equal harder
        (Api.Album.album_of_json
           (Yojson.Basic.from_file "test/test_data/htc.json")) );
    ( "Get album artist test" >:: fun _ ->
      assert_equal "Jimmy Cliff" (Api.Album.get_album_artists harder) );
    ( "Track number to name 1 test" >:: fun _ ->
      assert_equal "You Can Get It If You Really Want"
        (Api.Album.track_num_to_name 1 harder_list) );
    ( "Track number to name shorter track out of bounds test" >:: fun _ ->
      assert_equal "track number not found?"
        (Api.Album.track_num_to_name 13 harder_list) );
  ]

let suite =
  "test suite for Spoticaml"
  >::: List.flatten [ album_test; artist_test; track_test ]

let _ = print_endline (Api.Album.track_num_to_name 15 rumors_list)
let _ = run_test_tt_main suite
