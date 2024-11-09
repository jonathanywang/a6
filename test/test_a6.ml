open OUnit2
open A6.Set

let test_is_empty tree expected = assert_equal (is_empty tree) expected
let test_mem elem tree expected = assert_equal (mem elem tree) expected

let tests =
  "test suite"
  >::: [
         ("test empty tree" >:: fun _ -> test_is_empty empty true);
         ( "test non-empty tree" >:: fun _ ->
           let tree = insert 1 empty in
           test_is_empty tree false );
         ("test mem empty tree" >:: fun _ -> test_mem 1 empty false);
         ("test mem non-empty tree" >:: fun _ -> 
            let tree = insert )
       ]

let _ = run_test_tt_main tests
