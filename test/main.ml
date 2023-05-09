open OUnit2
open Api

let album_test = []
let artist_test = []
let track_test = []

let suite =
  "test suite for Spoticaml"
  >::: List.flatten [ album_test; artist_test; track_test ]

let _ = run_test_tt_main suite
