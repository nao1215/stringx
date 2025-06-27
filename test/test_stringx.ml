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
    ]
