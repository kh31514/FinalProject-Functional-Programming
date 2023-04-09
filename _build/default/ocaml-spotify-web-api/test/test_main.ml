open OUnit

let base_suite =
  "base_suite" >:::
    [
      Test_href.suite;
      Test_parse.suite;
    ]

let () = OUnit2.run_test_tt_main (ounit2_of_ounit1 base_suite)
