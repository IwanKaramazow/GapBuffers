open OUnit2;

let makeTest _ => {
  let gb = Gapbuffers.make gapCapacity::128 256;
  assert_equal gb.size 256;
  assert_equal gb.pre 0;
  assert_equal gb.post 128;
  assert_equal gb.gapCapacity 128
};

let moveCursorForwardTest _ => {
  let gb = Gapbuffers.make gapCapacity::128 256;
  let gb = Gapbuffers.moveCursorForward gb;
  assert_equal gb.pre 1;
  assert_equal gb.post 129
};

let moveCursorBackwardTest _ => {
  let gb = Gapbuffers.make gapCapacity::128 256;
  let gb =
    gb |> Gapbuffers.moveCursorForward |> Gapbuffers.moveCursorForward |> Gapbuffers.moveCursorBackward;
  assert_equal gb.pre 1;
  assert_equal gb.post 129
};

let insertCharTest _ => {
  let gb = Gapbuffers.make gapCapacity::128 256;
  let gb = Gapbuffers.insertChar gb 'a';
  print_endline "result:";
  print_char (Bytes.get gb.buffer 0);
  print_endline "";
  assert_equal (Bytes.get gb.buffer 0) 'a'
};

let suite =
  "Gapbuffer tests" >::: [
    "make returns a valid gapbuffer" >:: makeTest,
    "moves cursor forward" >:: moveCursorForwardTest,
    "moves cursor backward" >:: moveCursorBackwardTest,
    "insert a char" >:: insertCharTest
  ];

let () = run_test_tt_main suite;
