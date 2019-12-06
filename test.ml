open OUnit2
open Server

(* 
  WRITE TEST COMMENT PARAGRAPH
*)

let user_tests = 
  "init user suite" >:::
  [
    (* used to wake firebase up *)
    "firebase buffer time">:: (fun _ ->
        let _ = Server.user_exists "test" in () ); 
    "check if test user exists before creation">:: (fun _ ->
        assert_equal false 
          (Server.user_exists "test") ); 
    "create test user">:: (fun _ ->
        assert_equal ()
          (Server.create_user "test" "test") );
    "check if test user exists after creation">:: (fun _ ->
        assert_equal true 
          (Server.user_exists "test") );
    "auth test user with wrong password">:: (fun _ ->
        assert_equal false
          (Server.auth "test" "test") );
    "delete test user">:: (fun _ ->
        assert_equal ()
          (Server.delete_user "test" ) ); 
    "try to auth test user">:: (fun _ ->
        assert_equal false
          (Server.auth "test" "test") );
    "check if test user exists after deletion">:: (fun _ ->
        assert_equal false 
          (Server.user_exists "test") );
  ]

let convo_tests = 
  "init convo suite" >:::
  [
    "check if convo exists before creation" >:: (fun _ ->
        assert_equal false
          (Server.conversation_exists "test1" "test2") );
    "create test convo" >:: (fun _ ->
        assert_equal ()
          (Server.add_msg "test1" "test2" "a") );
    (* this is used as a buffer so firebase has time to update  *)
    "firebase buffer time" >:: (fun _ ->
        let _ = Server.conversation_exists "test1" "test2" in 
        let _ = Server.conversation_exists "test1" "test2" in 
        let _ = Server.conversation_exists "test1" "test2" in 
        let _ = Server.conversation_exists "test1" "test2" in
        let _ = Server.conversation_exists "test1" "test2" in 
        let _ = Server.conversation_exists "test1" "test2" in
        () );
    "check if convo exists a bit after creation" >:: (fun _ ->
        assert_equal true
          (Server.conversation_exists "test1" "test2") );
    "add message">:: (fun _ ->
        assert_equal ()
          (Server.add_msg "test2" "test1" "b") );
    "check convo between test2 and test1">:: (fun _ ->
        assert_equal ["test2: b"; "test1: a"]
          (Server.get_conversation_history "test2" "test1") );
    "check convo between test1 and test2 (should be same as previous)">:: (fun _ ->
        assert_equal ["test2: b"; "test1: a"]
          (Server.get_conversation_history "test1" "test2") );
    "delete convo and test users" >:: (fun _ ->
        assert_equal ()
          (Server.delete_conversation "test1" "test2"); 
        let _ = Server.delete_user "test1" in 
        let _ = Server.delete_user "test2" in () );
    (* this is used as a buffer so firebase has time to update  *)
    "firebase buffer time" >:: (fun _ ->
        let _ = Server.conversation_exists "test1" "test2" in 
        let _ = Server.conversation_exists "test1" "test2" in 
        let _ = Server.conversation_exists "test1" "test2" in 
        let _ = Server.conversation_exists "test1" "test2" in
        let _ = Server.conversation_exists "test1" "test2" in
        let _ = Server.conversation_exists "test1" "test2" in
        let _ = Server.conversation_exists "test1" "test2" in
        () );
    "check if convo exists a bit after deletion" >:: (fun _ ->
        assert_equal false
          (Server.conversation_exists "test1" "test2") );
  ]

let gc_test = 
  "init convo suite" >:::
  [
    "check if gc exists before creation" >:: (fun _ ->
        assert_equal false
          (Server.gc_exists "test_gc") );
    "create test gc" >:: (fun _ ->
        assert_equal ()
          (Server.create_gc "test_gc" ["test1";"test2"]) );
    (* this is used as a buffer so firebase has time to update  *)
    "firebase buffer time" >:: (fun _ ->
        let _ = Server.conversation_exists "test1" "test2" in 
        let _ = Server.conversation_exists "test1" "test2" in 
        let _ = Server.conversation_exists "test1" "test2" in 
        let _ = Server.conversation_exists "test1" "test2" in
        () );
    "check if gc exists after creation" >:: (fun _ ->
        assert_equal true
          (Server.gc_exists "test_gc") );
    "get gc users" >:: (fun _ ->
        assert_equal ["test1"; "test2"]
          (Server.get_gc_users "test_gc")); 
    "add message" >:: (fun _ ->
        assert_equal ()
          (Server.add_gc_msg "test_gc" "test1" "this is test1 saying hi there!")); 
    (* this is used as a buffer so firebase has time to update  *)
    "firebase buffer time" >:: (fun _ ->
        let _ = Server.get_gc_history "test_gc" in 
        let _ = Server.get_gc_history "test_gc" in 
        let _ = Server.get_gc_history "test_gc" in 
        let _ = Server.get_gc_history "test_gc" in
        () );
    "check gc history after first message" >:: (fun _ ->
        assert_equal ["test1: this is test1 saying hi there!"]
          (Server.get_gc_history "test_gc")); 
    "add message" >:: (fun _ ->
        assert_equal ()
          (Server.add_gc_msg "test_gc" "test2" "test2 says bye."));
    (* this is used as a buffer so firebase has time to update  *)
    "firebase buffer time" >:: (fun _ ->
        let _ = Server.get_gc_history "test_gc" in 
        let _ = Server.get_gc_history "test_gc" in 
        let _ = Server.get_gc_history "test_gc" in 
        let _ = Server.get_gc_history "test_gc" in
        () );
    "check gc history after second message" >:: (fun _ ->
        assert_equal ["test2: test2 says bye."; 
                      "test1: this is test1 saying hi there!"]
          (Server.get_gc_history "test_gc")); 
    "delete gc" >:: (fun _ ->
        assert_equal ()
          (Server.delete_gc "test_gc")); 
    (* this is used as a buffer so firebase has time to update  *)
    "firebase buffer time" >:: (fun _ ->
        let _ = Server.gc_exists "test_gc" in 
        let _ = Server.gc_exists "test_gc" in 
        let _ = Server.gc_exists "test_gc" in 
        let _ = Server.gc_exists "test_gc" in
        () );
    "check if gc exists after deletion" >:: (fun _ ->
        assert_equal false
          (Server.gc_exists "test_gc") );

  ]

let tests =
  [
    user_tests;
    convo_tests;  
    gc_test; 
  ]

let suite =
  "test suite"  >::: List.flatten [
    tests
  ]

let _ = run_test_tt_main suite