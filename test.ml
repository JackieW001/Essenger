open OUnit2
open Server

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
        () );
    "check if convo exists a bit after creation" >:: (fun _ ->
        assert_equal true
          (Server.conversation_exists "test1" "test2") );
    "add message">:: (fun _ ->
        assert_equal ()
          (Server.add_msg "test2" "test1" "b") );
    "print convo between test2 and test1">:: (fun _ ->
        assert_equal ()
          (Server.get_conversation_history "test2" "test1" 5) );
    "print convo between test1 and test2 (should be same as previous)">:: (fun _ ->
        assert_equal ()
          (Server.get_conversation_history "test1" "test2" 5) );
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
        () );
    "check if convo exists a bit after deletion" >:: (fun _ ->
        assert_equal false
          (Server.conversation_exists "test1" "test2") );
  ]

let tests =
  [
    user_tests;
    convo_tests;
  ]

let suite =
  "test suite"  >::: List.flatten [
    tests
  ]

let _ = run_test_tt_main suite