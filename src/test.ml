open OUnit2
open Player

       
let suite =
"suite">:::
 Parser.tests
;;

let () =
  run_test_tt_main suite
;;
