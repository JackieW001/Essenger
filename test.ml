open OUnit2
open Server

(* 
  These test cases test the interaction between our [server.ml] file
  and Firebase. Specifically, we check if mutating users, group chats,
  and conversations are handled properly. 
  Note: gc stands for group chat
  For each individual test suite (user_test, convo_test, or gc_tests), we 
  check if the target (user, convo, or gc) is 
  1. Non existent since it hasn't been created yet
  2. Created and added into Firebase
  3. Existent, since we just created the target
  4. Target specific functions that will be discussed afterwards
  5. Deleted from Firebase
  6. Non existent since we just deleted the target

Target specific functions
user
  - auth: check to make sure authentification is done properly. Specifically, 
  a user can not login with an incorrect password
convo 
  - add_msg: adds message to conversation between two users in firebase
  - get_conversation_history: checks if a list of strings with the newest
  conversations being at the front of the list is returned
   -get_notifications: check to make sure notifications are updated properly. 
gc 
  - add_gc_msg: adds message to group chat in firebase
  - get_gc_history: checks if a list of strings with the newest
  conversations being at the front of the list is returned

Tests with a comment above them are used for buffer time so that Firebase
has time to update. When running this test file, tests involving deletions 
(i.e. deleting information in Firebase). This means that Firebase was not
updated in time of the function call. Although the test case may fail, 
the target is indeed deleted from Firebase; the function call was just run 
too early. To solve this issue, we added some "dummy" Firebase calls so
that Firebase has time to update. 

Note: Due to Firebase's long latency time, some of the fuctions that 
update Firebase will take a long time, resulting in tests that query Firebase
right after updating Firebase may result in failure. Running this test
file multiple times will result in varying number of failures and errors
due to Firebase's latency update time. We have included a screenshot of all the
test cases working in our MS3 progress report. 

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
    (* this is used as a buffer so firebase has time to update  *)
    "firebase buffer time" >:: (fun _ ->
        for x = 0 to 5 do 
          let _ = Server.conversation_exists "test1" "test2" in ()
        done );
    "check if test user exists after creation">:: (fun _ ->
        assert_equal true 
          (Server.user_exists "test") );
    "auth test user with wrong password">:: (fun _ ->
        assert_equal false
          (Server.auth "test" "test") );
    "delete test user">:: (fun _ ->
        assert_equal ()
          (Server.delete_user "test" ) ); 
    (* this is used as a buffer so firebase has time to update  *)
    "firebase buffer time" >:: (fun _ ->
        for x = 0 to 5 do 
          let _ = Server.conversation_exists "test1" "test2" in ()
        done );
    "try to auth test user">:: (fun _ ->
        assert_equal false
          (Server.auth "test" "test") );
    (* this is used as a buffer so firebase has time to update  *)
    "firebase buffer time" >:: (fun _ ->
        for x = 0 to 5 do 
          let _ = Server.conversation_exists "test1" "test2" in ()
        done );
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
    "check for test1 notification">:: (fun _ ->
        assert_equal ["test2"]
          (Server.get_notifications "test1" ));
    "check convo between test2 and test1">:: (fun _ ->
        assert_equal ["test2: b"; "test1: a"]
          (Server.get_conversation_history "test2" "test1") );
    "check convo between test1 and test2 (should be same as previous)">:: (fun _ ->
        assert_equal ["test2: b"; "test1: a"]
          (Server.get_conversation_history "test1" "test2") );
    (* this is used as a buffer so firebase has time to update  *)
    "firebase buffer time" >:: (fun _ ->
        for x = 0 to 5 do 
          let _ = Server.conversation_exists "test1" "test2" in ()
        done );
    "check for test1 notification after get convo">:: (fun _ ->
        assert_equal []
          (Server.get_notifications "test1" ));
    "delete convo and test users" >:: (fun _ ->
        assert_equal ()
          (Server.delete_conversation "test1" "test2"); 
        let _ = Server.delete_user "test1" in 
        let _ = Server.delete_user "test2" in () );
    (* this is used as a buffer so firebase has time to update  *)
    "firebase buffer time" >:: (fun _ ->
        for x = 0 to 15 do 
          let _ = Server.conversation_exists "test1" "test2" in ()
        done );
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