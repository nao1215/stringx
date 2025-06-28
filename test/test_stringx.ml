open Alcotest

let test_distance () =
  let open Stringx.Levenshtein in
  check int "kitten vs sitting" 3 (distance "kitten" "sitting");
  check int "book vs back" 2 (distance "book" "back");
  check int "あいう vs あいえ" 1 (distance "あいう" "あいえ");
  check int "犬 vs 猫" 1 (distance "犬" "猫");
  check int "🍎 vs 🍏" 1 (distance "🍎" "🍏");
  check int "𠮷野家 vs 吉野家" 1 (distance "𠮷野家" "吉野家");
  check int "空白 vs 空" 1 (distance "空白" "空")

let test_center () =
  let open Stringx in
  Alcotest.(check string)
    "shorter than length" "  hello   " (center "hello" 10 " ");
  Alcotest.(check string) "longer than length" "hello" (center "hello" 4 " ");
  Alcotest.(check string) "unicode pad" "ああhelloああ" (center "hello" 9 "あ");
  Alcotest.(check string) "empty pad" "hello" (center "hello" 10 "");
  Alcotest.(check string)
    "multi-byte pad" "12hello123" (center "hello" 10 "123")

let test_count () =
  let open Stringx in
  Alcotest.(check int) "count vowels" 2 (count "hello" "aeiou");
  Alcotest.(check int) "count range" 2 (count "hello" "a-k");
  Alcotest.(check int) "count negation" 3 (count "hello" "^aeiou");
  Alcotest.(check int) "count utf8 match" 3 (count "あいaうえ" "aうえ");
  Alcotest.(check int) "count utf8 negation" 3 (count "あいaうえ" "^あい");
  Alcotest.(check int) "count with range unicode" 3 (count "abcABC" "A-Z");
  Alcotest.(check int) "count with full match" 5 (count "あいうえお" "あいうえお");
  Alcotest.(check int) "count with empty pattern" 0 (count "abc" "");
  Alcotest.(check int) "count with empty string" 0 (count "" "abc")

let test_delete () =
  let open Stringx in
  Alcotest.(check string) "delete vowels" "hll" (delete "hello" "aeiou");
  Alcotest.(check string) "delete range a-k" "llo" (delete "hello" "a-k");
  Alcotest.(check string) "delete ^a-k" "he" (delete "hello" "^a-k");
  Alcotest.(check string) "delete unicode emoji" "🍏" (delete "🍎🍏" "🍎");
  Alcotest.(check string) "delete Japanese" "んにちは" (delete "こんにちは" "こ");
  Alcotest.(check string) "delete all" "" (delete "abc" "a-c");
  Alcotest.(check string) "delete nothing" "abc" (delete "abc" "xyz");
  Alcotest.(check string) "delete ^" "aeiou" (delete "aeiou" "^aeiou")

let test_len () =
  let open Stringx in
  Alcotest.(check int) "ascii" 5 (len "hello");
  Alcotest.(check int) "hiragana" 5 (len "こんにちは");
  Alcotest.(check int) "emoji" 3 (len "🍎🍏🍊");
  Alcotest.(check int) "empty" 0 (len "");
  Alcotest.(check int) "mixed ascii/emoji" 5 (len "a🍎b🍏c");
  Alcotest.(check int) "malformed utf8" 3 (len "a\xffb")

let test_reverse () =
  let open Stringx in
  Alcotest.(check string) "ascii" "olleh" (reverse "hello");
  Alcotest.(check string) "hiragana" "はちにんこ" (reverse "こんにちは");
  Alcotest.(check string) "empty" "" (reverse "");
  Alcotest.(check string) "single char" "a" (reverse "a");
  Alcotest.(check string) "emoji" "🍊🍏🍎" (reverse "🍎🍏🍊");
  Alcotest.(check string) "mixed ascii/emoji" "c🍏b🍎a" (reverse "a🍎b🍏c");
  Alcotest.(check string) "combining marks" "́éa" (reverse "áé")

let test_contains () =
  let open Stringx in
  Alcotest.(check bool)
    "contains: foo in seafood" true (contains "seafood" "foo");
  Alcotest.(check bool)
    "contains: bar in seafood" false (contains "seafood" "bar");
  Alcotest.(check bool)
    "contains: empty in seafood" true (contains "seafood" "");
  Alcotest.(check bool) "contains: empty in empty" true (contains "" "");
  Alcotest.(check bool) "contains: non-empty in empty" false (contains "" "a");
  Alcotest.(check bool) "contains: full match" true (contains "abc" "abc");
  Alcotest.(check bool) "contains: partial match" true (contains "abcde" "bcd");
  Alcotest.(check bool) "contains: unicode match" true (contains "こんにちは" "にち");
  Alcotest.(check bool)
    "contains: unicode no match" false
    (contains "こんにちは" "さようなら")

let test_contains_any () =
  let open Stringx in
  Alcotest.(check bool) "team/i" false (contains_any "team" "i");
  Alcotest.(check bool) "fail/ui" true (contains_any "fail" "ui");
  Alcotest.(check bool) "ure/ui" true (contains_any "ure" "ui");
  Alcotest.(check bool) "failure/ui" true (contains_any "failure" "ui");
  Alcotest.(check bool) "foo/empty" false (contains_any "foo" "");
  Alcotest.(check bool) "empty/empty" false (contains_any "" "");
  Alcotest.(check bool) "unicode/emoji" true (contains_any "🍎🍏🍊" "🍏");
  Alcotest.(check bool) "unicode/no match" false (contains_any "こんにちは" "さよ");
  Alcotest.(check bool) "unicode/match" true (contains_any "こんにちは" "ちに")

let test_has_prefix () =
  let open Stringx in
  Alcotest.(check bool) "ascii: Go" true (has_prefix "Gopher" "Go");
  Alcotest.(check bool) "ascii: C" false (has_prefix "Gopher" "C");
  Alcotest.(check bool) "ascii: empty" true (has_prefix "Gopher" "");
  Alcotest.(check bool) "empty: empty" true (has_prefix "" "");
  Alcotest.(check bool) "empty: non-empty" false (has_prefix "" "a");
  Alcotest.(check bool) "full match" true (has_prefix "abc" "abc");
  Alcotest.(check bool) "partial match" true (has_prefix "abcde" "abc");
  Alcotest.(check bool) "unicode match" true (has_prefix "こんにちは" "こん");
  Alcotest.(check bool) "unicode no match" false (has_prefix "こんにちは" "さよ");
  Alcotest.(check bool) "emoji match" true (has_prefix "🍎🍏🍊" "🍎");
  Alcotest.(check bool) "emoji no match" false (has_prefix "🍎🍏🍊" "🍊")

let test_has_suffix () =
  let open Stringx in
  Alcotest.(check bool) "ascii: go" true (has_suffix "Amigo" "go");
  Alcotest.(check bool) "ascii: O" false (has_suffix "Amigo" "O");
  Alcotest.(check bool) "ascii: Ami" false (has_suffix "Amigo" "Ami");
  Alcotest.(check bool) "ascii: empty" true (has_suffix "Amigo" "");
  Alcotest.(check bool) "empty: empty" true (has_suffix "" "");
  Alcotest.(check bool) "empty: non-empty" false (has_suffix "" "a");
  Alcotest.(check bool) "full match" true (has_suffix "abc" "abc");
  Alcotest.(check bool) "partial match" true (has_suffix "abcde" "cde");
  Alcotest.(check bool) "unicode match" true (has_suffix "こんにちは" "ちは");
  Alcotest.(check bool) "unicode no match" false (has_suffix "こんにちは" "さよ");
  Alcotest.(check bool) "emoji match" true (has_suffix "🍎🍏🍊" "🍊");
  Alcotest.(check bool) "emoji no match" false (has_suffix "🍎🍏🍊" "🍎")

let test_count_substring () =
  let open Stringx in
  Alcotest.(check int) "cheese/e" 3 (count_substring "cheese" "e");
  Alcotest.(check int) "five/empty" 5 (count_substring "five" "");
  Alcotest.(check int) "banana/na" 2 (count_substring "banana" "na");
  Alcotest.(check int) "aaaaa/aa" 2 (count_substring "aaaaa" "aa");
  Alcotest.(check int) "empty/empty" 1 (count_substring "" "");
  Alcotest.(check int) "empty/a" 0 (count_substring "" "a");
  Alcotest.(check int) "abc/abc" 1 (count_substring "abc" "abc");
  Alcotest.(check int) "abc/abcd" 0 (count_substring "abc" "abcd");
  Alcotest.(check int) "unicode/に" 1 (count_substring "こんにちは" "に");
  Alcotest.(check int) "unicode/ん" 1 (count_substring "こんにちは" "ん");
  Alcotest.(check int) "unicode/empty" 6 (count_substring "こんにちは" "");
  Alcotest.(check int) "emoji/🍏" 1 (count_substring "🍎🍏🍊" "🍏");
  Alcotest.(check int) "emoji/🍎🍏" 1 (count_substring "🍎🍏🍊" "🍎🍏")

let test_equal_fold () =
  let open Stringx in
  Alcotest.(check bool) "Go/go" true (equal_fold "Go" "go");
  Alcotest.(check bool) "AB/ab" true (equal_fold "AB" "ab");
  Alcotest.(check bool) "ß/ss" false (equal_fold "ß" "ss");
  Alcotest.(check bool) "ascii/ASCII" true (equal_fold "ASCII" "ascii");
  Alcotest.(check bool) "hiragana" false (equal_fold "あ" "ア");
  Alcotest.(check bool) "emoji" false (equal_fold "🍎" "🍏");
  Alcotest.(check bool) "empty" true (equal_fold "" "");
  Alcotest.(check bool) "mixed" false (equal_fold "Go" "Go!")

let test_fields () =
  let open Stringx in
  Alcotest.(check (list string))
    "basic" [ "foo"; "bar"; "baz" ]
    (fields "  foo bar  baz   ");
  Alcotest.(check (list string))
    "tabs/newlines" [ "a"; "b"; "c" ] (fields "a\tb\nc");
  Alcotest.(check (list string))
    "unicode space" [ "a"; "b" ] (fields "a\u{3000}b");
  Alcotest.(check (list string)) "only spaces" [] (fields "   ");
  Alcotest.(check (list string)) "empty" [] (fields "");
  Alcotest.(check (list string)) "single word" [ "word" ] (fields "word");
  Alcotest.(check (list string))
    "leading/trailing" [ "word" ] (fields "  word  ");
  Alcotest.(check (list string)) "multi space" [ "a"; "b" ] (fields " a  b ");
  Alcotest.(check (list string)) "emoji" [ "🍎"; "🍏" ] (fields "🍎  🍏");
  Alcotest.(check (list string))
    "mixed" [ "foo"; "🍏"; "bar" ] (fields "foo  🍏  bar")

let is_letter_or_number u =
  let open Uchar in
  let c = to_int u in
  (* Basic Latin letters and numbers *)
  (c >= 0x30 && c <= 0x39)
  || (c >= 0x41 && c <= 0x5A)
  || (c >= 0x61 && c <= 0x7A)
(* You can extend this for full Unicode if needed *)

let test_fields_func () =
  let open Stringx in
  let f u = not (is_letter_or_number u) in
  Alcotest.(check (list string))
    "fields_func basic" [ "foo1"; "bar2"; "baz3" ]
    (fields_func "  foo1;bar2,baz3..." f);
  Alcotest.(check (list string))
    "fields_func only sep" [] (fields_func ";;;;" f);
  Alcotest.(check (list string)) "fields_func empty" [] (fields_func "" f);
  Alcotest.(check (list string))
    "fields_func single" [ "abc123" ] (fields_func "abc123" f);
  Alcotest.(check (list string))
    "fields_func unicode"
    [ "こんにちは"; "123" ]
    (fields_func "こんにちは,123" (fun u -> Uchar.to_int u = 0x2c));
  Alcotest.(check (list string))
    "fields_func emoji" [ "🍎🍏"; "🍊" ]
    (fields_func "🍎🍏,🍊" (fun u -> Uchar.to_int u = 0x2c))

let test_index () =
  let open Stringx in
  Alcotest.(check int) "chicken/ken" 4 (index "chicken" "ken");
  Alcotest.(check int) "chicken/dmr" (-1) (index "chicken" "dmr");
  Alcotest.(check int) "abc/empty" 0 (index "abc" "");
  Alcotest.(check int) "empty/empty" 0 (index "" "");
  Alcotest.(check int) "empty/a" (-1) (index "" "a");
  Alcotest.(check int) "abc/abc" 0 (index "abc" "abc");
  Alcotest.(check int) "abc/abcd" (-1) (index "abc" "abcd")

let test_repeat () =
  let open Stringx in
  Alcotest.(check string) "repeat na 2" "nana" (repeat "na" 2);
  Alcotest.(check string) "repeat 🍎 3" "🍎🍎🍎" (repeat "🍎" 3);
  Alcotest.(check string) "repeat empty 5" "" (repeat "" 5);
  Alcotest.(check string) "repeat a 0" "" (repeat "a" 0);
  Alcotest.check_raises "repeat negative"
    (Invalid_argument "repeat: negative count") (fun () ->
      ignore (repeat "abc" (-1)))

let test_join () =
  let open Stringx in
  Alcotest.(check string)
    "join basic" "foo, bar, baz"
    (join [ "foo"; "bar"; "baz" ] ", ");
  Alcotest.(check string) "join empty list" "" (join [] ", ");
  Alcotest.(check string) "join single" "a" (join [ "a" ] ", ");
  Alcotest.(check string) "join unicode" "🍎-🍏-🍊" (join [ "🍎"; "🍏"; "🍊" ] "-");
  Alcotest.(check string)
    "join empty sep" "foobarbaz"
    (join [ "foo"; "bar"; "baz" ] "");
  Alcotest.(check string) "join all empty" "" (join [] "")

let test_trim () =
  let open Stringx in
  Alcotest.(check string)
    "ascii trim" "Hello, Camels"
    (trim "¡¡¡Hello, Camels!!!" "!¡");
  Alcotest.(check string) "trim nothing" "hello" (trim "hello" "");
  Alcotest.(check string) "trim all" "" (trim "aaa" "a");
  Alcotest.(check string) "trim unicode" "b" (trim "ああbあ" "あ");
  Alcotest.(check string) "trim both sides" "b" (trim "xybxy" "xy");
  Alcotest.(check string) "trim only leading" "abc" (trim "!!abc" "!");
  Alcotest.(check string) "trim only trailing" "abc" (trim "abc!!" "!");
  Alcotest.(check string) "trim empty string" "" (trim "" "!");
  Alcotest.(check string) "trim emoji" "b" (trim "🍎b🍎" "🍎")

let is_letter_or_number u =
  let c = Uchar.to_int u in
  (* Basic Latin letters and numbers *)
  (c >= 0x30 && c <= 0x39)
  || (c >= 0x41 && c <= 0x5A)
  || (c >= 0x61 && c <= 0x7A)
(* You can extend this for full Unicode if needed *)

let test_trim_func () =
  let open Stringx in
  let f u = not (is_letter_or_number u) in
  Alcotest.(check string)
    "trim_func ascii/unicode" "Hello, Camels"
    (trim_func "¡¡¡Hello, Camels!!!" f);
  Alcotest.(check string)
    "trim_func nothing" "hello"
    (trim_func "hello" (fun _ -> false));
  Alcotest.(check string) "trim_func all" "" (trim_func "aaa" (fun _ -> true));
  Alcotest.(check string)
    "trim_func unicode" "b"
    (trim_func "ああbあ" (fun u -> Uchar.to_int u = 0x3042));
  Alcotest.(check string)
    "trim_func emoji" "b"
    (trim_func "🍎b🍎" (fun u -> Uchar.to_int u = 0x1F34E));
  Alcotest.(check string) "trim_func empty" "" (trim_func "" f)

let test_trim_left () =
  let open Stringx in
  Alcotest.(check string)
    "ascii trim_left" "Hello, Camels!!!"
    (trim_left "¡¡¡Hello, Camels!!!" "!¡");
  Alcotest.(check string) "trim_left nothing" "hello" (trim_left "hello" "");
  Alcotest.(check string) "trim_left all" "" (trim_left "aaa" "a");
  Alcotest.(check string) "trim_left unicode" "bあ" (trim_left "ああbあ" "あ");
  Alcotest.(check string) "trim_left both sides" "bxy" (trim_left "xybxy" "xy");
  Alcotest.(check string) "trim_left only leading" "abc" (trim_left "!!abc" "!");
  Alcotest.(check string)
    "trim_left only trailing" "abc!!" (trim_left "abc!!" "!");
  Alcotest.(check string) "trim_left empty string" "" (trim_left "" "!");
  Alcotest.(check string) "trim_left emoji" "b🍎" (trim_left "🍎b🍎" "🍎")

let test_trim_left_func () =
  let open Stringx in
  let f u = not (is_letter_or_number u) in
  Alcotest.(check string)
    "trim_left_func ascii/unicode" "Hello, Camels!!!"
    (trim_left_func "¡¡¡Hello, Camels!!!" f);
  Alcotest.(check string)
    "trim_left_func nothing" "hello"
    (trim_left_func "hello" (fun _ -> false));
  Alcotest.(check string)
    "trim_left_func all" ""
    (trim_left_func "aaa" (fun _ -> true));
  Alcotest.(check string)
    "trim_left_func unicode" "bあ"
    (trim_left_func "ああbあ" (fun u -> Uchar.to_int u = 0x3042));
  Alcotest.(check string)
    "trim_left_func emoji" "b🍎"
    (trim_left_func "🍎b🍎" (fun u -> Uchar.to_int u = 0x1F34E));
  Alcotest.(check string) "trim_left_func empty" "" (trim_left_func "" f)

let test_trim_right () =
  let open Stringx in
  Alcotest.(check string)
    "ascii trim_right" "¡¡¡Hello, Camels"
    (trim_right "¡¡¡Hello, Camels!!!" "!¡");
  Alcotest.(check string) "trim_right nothing" "hello" (trim_right "hello" "");
  Alcotest.(check string) "trim_right all" "" (trim_right "aaa" "a");
  Alcotest.(check string) "trim_right unicode" "ああb" (trim_right "ああbあ" "あ");
  Alcotest.(check string)
    "trim_right both sides" "xyb" (trim_right "xybxy" "xy");
  Alcotest.(check string)
    "trim_right only trailing" "abc" (trim_right "abc!!" "!");
  Alcotest.(check string)
    "trim_right only leading" "!!abc" (trim_right "!!abc" "!");
  Alcotest.(check string) "trim_right empty string" "" (trim_right "" "!");
  Alcotest.(check string) "trim_right emoji" "🍎b" (trim_right "🍎b🍎" "🍎")

let test_trim_right_func () =
  let open Stringx in
  let f u = not (is_letter_or_number u) in
  Alcotest.(check string)
    "trim_right_func ascii/unicode" "¡¡¡Hello, Camels"
    (trim_right_func "¡¡¡Hello, Camels!!!" f);
  Alcotest.(check string)
    "trim_right_func nothing" "hello"
    (trim_right_func "hello" (fun _ -> false));
  Alcotest.(check string)
    "trim_right_func all" ""
    (trim_right_func "aaa" (fun _ -> true));
  Alcotest.(check string)
    "trim_right_func unicode" "ああb"
    (trim_right_func "ああbあ" (fun u -> Uchar.to_int u = 0x3042));
  Alcotest.(check string)
    "trim_right_func emoji" "🍎b"
    (trim_right_func "🍎b🍎" (fun u -> Uchar.to_int u = 0x1F34E));
  Alcotest.(check string) "trim_right_func empty" "" (trim_right_func "" f)

let test_trim_space () =
  let open Stringx in
  Alcotest.(check string)
    "ascii trim_space" "Hello, Camels"
    (trim_space " \t\n Hello, Camels \n\t\r\n");
  Alcotest.(check string)
    "unicode trim_space" "Hello, Camels"
    (trim_space "\u{3000}Hello, Camels\u{3000}");
  Alcotest.(check string) "no trim_space" "Hello" (trim_space "Hello");
  Alcotest.(check string) "all space" "" (trim_space " \t\n\u{3000}");
  Alcotest.(check string) "empty" "" (trim_space "");
  Alcotest.(check string) "emoji no trim" "🍎🍏🍊" (trim_space "🍎🍏🍊")

let test_trim_suffix () =
  let open Stringx in
  Alcotest.(check string)
    "ascii: , Camels!!!" "¡¡¡Hello"
    (trim_suffix "¡¡¡Hello, Camels!!!" ", Camels!!!");
  Alcotest.(check string)
    "ascii: , Marmots!!!" "¡¡¡Hello, Camels!!!"
    (trim_suffix "¡¡¡Hello, Camels!!!" ", Marmots!!!");
  Alcotest.(check string) "empty suffix" "abc" (trim_suffix "abc" "");
  Alcotest.(check string) "empty string" "" (trim_suffix "" "abc");
  Alcotest.(check string) "full match" "" (trim_suffix "abc" "abc");
  Alcotest.(check string) "partial match" "abc" (trim_suffix "abcde" "de");
  Alcotest.(check string) "unicode match" "こんに" (trim_suffix "こんにちは" "ちは");
  Alcotest.(check string) "unicode no match" "こんにちは" (trim_suffix "こんにちは" "さよ");
  Alcotest.(check string) "emoji match" "🍎🍏" (trim_suffix "🍎🍏🍊" "🍊");
  Alcotest.(check string) "emoji no match" "🍎🍏🍊" (trim_suffix "🍎🍏🍊" "🍎")

let () =
  run "stringx"
    [
      ( "UTF-8 distance tests",
        [ test_case "basic distances" `Quick test_distance ] );
      ("center tests", [ test_case "center basic" `Quick test_center ]);
      ("count tests", [ test_case "count basic" `Quick test_count ]);
      ("delete tests", [ test_case "delete basic" `Quick test_delete ]);
      ("length tests", [ test_case "length basic" `Quick test_len ]);
      ("reverse tests", [ test_case "reverse basic" `Quick test_reverse ]);
      ("contains tests", [ test_case "contains basic" `Quick test_contains ]);
      ( "contains any tests",
        [ test_case "contains any basic" `Quick test_contains_any ] );
      ( "has prefix tests",
        [ test_case "has prefix basic" `Quick test_has_prefix ] );
      ( "has suffix tests",
        [ test_case "has suffix basic" `Quick test_has_suffix ] );
      ( "count substring tests",
        [ test_case "count substring basic" `Quick test_count_substring ] );
      ( "equal fold tests",
        [ test_case "equal fold basic" `Quick test_equal_fold ] );
      ("fields tests", [ test_case "fields basic" `Quick test_fields ]);
      ( "fields func tests",
        [ test_case "fields func basic" `Quick test_fields_func ] );
      ("index tests", [ test_case "index basic" `Quick test_index ]);
      ("repeat tests", [ test_case "repeat basic" `Quick test_repeat ]);
      ("join tests", [ test_case "join basic" `Quick test_join ]);
      ("trim tests", [ test_case "trim basic" `Quick test_trim ]);
      ("trim func tests", [ test_case "trim func basic" `Quick test_trim_func ]);
      ("trim left tests", [ test_case "trim left basic" `Quick test_trim_left ]);
      ( "trim left func tests",
        [ test_case "trim left func basic" `Quick test_trim_left_func ] );
      ( "trim right tests",
        [ test_case "trim right basic" `Quick test_trim_right ] );
      ( "trim right func tests",
        [ test_case "trim right func basic" `Quick test_trim_right_func ] );
      ( "trim space tests",
        [ test_case "trim space basic" `Quick test_trim_space ] );
      ( "trim suffix tests",
        [ test_case "trim suffix basic" `Quick test_trim_suffix ] );
    ]
