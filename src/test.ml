open OUnit2
open Paths
       
let suite =
"suite">:::
 MapIO.tests @ Priority.tests @ Hex.tests @ Bitset.tests
;;

let () =
  run_test_tt_main suite
;;
