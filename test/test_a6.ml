open OUnit2
open A6.Set

(* Test helpers using to_string for comparison *)
let test_is_empty tree expected = assert_equal (is_empty tree) expected
let test_mem elem tree expected = assert_equal (mem elem tree) expected

(* Helper to test insertions using to_string for easier comparison *)
let test_insert elem tree expected_tree =
  let actual = to_string string_of_int (insert elem tree) in
  let expected = to_string string_of_int expected_tree in
  assert_equal actual expected

let tests =
  "test suite"
  >::: [
         (* Testing 1. is_empty empty = true *)
         ("test empty tree" >:: fun _ -> test_is_empty empty true);
         (* Testing 2. is_empty (insert x s) = false *)
         ( "test non-empty tree" >:: fun _ ->
           let tree = insert 1 empty in
           test_is_empty tree false );
         (* Testing 3. mem x empty = false *)
         ("test mem empty tree" >:: fun _ -> test_mem 1 empty false);
         (* Testing 4. mem y (insert x s) = true if x = y *)
         ( "test mem non-empty tree true" >:: fun _ ->
           let tree = insert 1 empty in
           test_mem 1 tree true );
         (* Testing 5. mem y (insert x s) = mem y s if x <> y *)
         ( "test mem non_empty tree false" >:: fun _ ->
           let tree = insert 2 empty in
           test_mem 1 tree false );
         (* Case: mem x in a ThreeNode where x = v1 *)
         ( "test mem ThreeNode where x = v1" >:: fun _ ->
           let tree =
             create_three_node 2 5 (insert 1 empty) (insert 3 empty)
               (insert 6 empty)
           in
           test_mem 2 tree true );
         (* Case: mem x in a ThreeNode where x = v2 *)
         ( "test mem ThreeNode where x = v2" >:: fun _ ->
           let tree =
             create_three_node 2 5 (insert 1 empty) (insert 3 empty)
               (insert 6 empty)
           in
           test_mem 5 tree true );
         (* Case: mem x in a ThreeNode where x < v1 *)
         ( "test mem ThreeNode where x < v1" >:: fun _ ->
           let tree =
             create_three_node 4 7 (insert 2 empty) (insert 5 empty)
               (insert 8 empty)
           in
           test_mem 2 tree true );
         (* Case: mem x in a ThreeNode where v1 < x < v2 *)
         ( "test mem ThreeNode where v1 < x < v2" >:: fun _ ->
           let tree =
             create_three_node 3 6 (insert 2 empty) (insert 4 empty)
               (insert 7 empty)
           in
           test_mem 4 tree true );
         (* Case: mem x in a ThreeNode where x > v2 *)
         ( "test mem ThreeNode where x > v2" >:: fun _ ->
           let tree =
             create_three_node 3 6 (insert 2 empty) (insert 4 empty)
               (insert 8 empty)
           in
           test_mem 8 tree true );
         (* Handling of duplicates *)
         (* Case: Insert duplicate into TwoNode *)
         ( "test insert duplicate into TwoNode" >:: fun _ ->
           let tree = create_two_node 5 empty empty in
           test_insert 5 tree tree );
         (* Case: Insert duplicate into ThreeNode *)
         ( "test insert duplicate into ThreeNode" >:: fun _ ->
           let tree = create_three_node 2 5 empty empty empty in
           test_insert 2 tree tree );
         (* Complex nested ThreeNodes insertions 1 *)
         ( "test complex insertions" >:: fun _ ->
           let tree = empty in
           let tree = insert 2 tree in
           let tree = insert 5 tree in
           let tree = insert 4 tree in
           let expected =
             create_two_node 4
               (create_two_node 2 empty empty)
               (create_two_node 5 empty empty)
           in
           assert_equal
             ~printer:(fun x -> x)
             (to_string string_of_int expected)
             (to_string string_of_int tree) );
         (* Complex nested ThreeNodes insertions 2 *)
         ( "test complex insertions" >:: fun _ ->
           let tree = empty in
           let tree = insert 2 tree in
           let tree = insert 5 tree in
           let tree = insert 8 tree in
           let expected =
             create_two_node 5
               (create_two_node 2 empty empty)
               (create_two_node 8 empty empty)
           in
           assert_equal
             ~printer:(fun x -> x)
             (to_string string_of_int expected)
             (to_string string_of_int tree) );
         (* Complex nested ThreeNodes insertions 3 *)
         ( "test complex insertions" >:: fun _ ->
           let tree = empty in
           let tree = insert 2 tree in
           let tree = insert 5 tree in
           let tree = insert 1 tree in
           let expected =
             create_two_node 2
               (create_two_node 1 empty empty)
               (create_two_node 5 empty empty)
           in
           assert_equal
             ~printer:(fun x -> x)
             (to_string string_of_int expected)
             (to_string string_of_int tree) );
         (* Complex nested ThreeNodes insertions 4 *)
         ( "test complex insertions" >:: fun _ ->
           let tree = empty in
           let tree = insert 5 tree in
           let tree = insert 1 tree in
           let tree = insert 2 tree in
           let tree = insert 4 tree in
           let tree = insert 3 tree in
           let expected =
             create_three_node 2 4
               (create_two_node 1 empty empty)
               (create_two_node 3 empty empty)
               (create_two_node 5 empty empty)
           in
           assert_equal
             ~printer:(fun x -> x)
             (to_string string_of_int expected)
             (to_string string_of_int tree) );
       ]

let _ = run_test_tt_main tests
