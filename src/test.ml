open OUnit2

       
let suite =
"suite">:::
 MapIO.tests
;;

let () =
  run_test_tt_main suite
;;
