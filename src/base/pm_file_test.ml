open Core.Std
open OUnit

let test_read_file _ =
  let file = Filename.temp_file "read_file" "test"
  in
  let fout = open_out file
  in
  output_string fout "testing";
  close_out fout;
  assert_equal "testing" (Pm_file.read_file file);
  Shell.rm file

let suite = "File test" >:::
  [ "Read file" >:: test_read_file ]

let _ = run_test_tt_main suite
