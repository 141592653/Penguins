open OUnit2

       
let suite =
"suite">:::
 MapIO.tests @ Priority.tests @ Hex.tests
;;

let () =
  run_test_tt_main suite
;;
