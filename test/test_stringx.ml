open Alcotest

let test_distance () =
  let open Stringx.Levenshtein in
  check int "kitten vs sitting" 3 (distance ~s:"kitten" ~t:"sitting");
  check int "book vs back" 2 (distance ~s:"book" ~t:"back");
  check int "あいう vs あいえ" 1 (distance ~s:"あいう" ~t:"あいえ");
  check int "犬 vs 猫" 1 (distance ~s:"犬" ~t:"猫");
  check int "🍎 vs 🍏" 1 (distance ~s:"🍎" ~t:"🍏");
  check int "𠮷野家 vs 吉野家" 1 (distance ~s:"𠮷野家" ~t:"吉野家");
  check int "空白 vs 空" 1 (distance ~s:"空白" ~t:"空")

let test_center () =
  let open Stringx in
  Alcotest.(check string)
    "shorter than length" "  hello   "
    (center ~len:10 ~pad:" " "hello");
  Alcotest.(check string)
    "longer than length" "hello"
    (center ~len:4 ~pad:" " "hello");
  Alcotest.(check string)
    "unicode pad" "ああhelloああ"
    (center ~len:9 ~pad:"あ" "hello");
  Alcotest.(check string) "empty pad" "hello" (center ~len:10 ~pad:"" "hello");
  Alcotest.(check string)
    "multi-byte pad" "12hello123"
    (center ~len:10 ~pad:"123" "hello")

let test_count () =
  let open Stringx in
  Alcotest.(check int) "count vowels" 2 (count ~pattern:"aeiou" "hello");
  Alcotest.(check int) "count range" 2 (count ~pattern:"a-k" "hello");
  Alcotest.(check int) "count negation" 3 (count ~pattern:"^aeiou" "hello");
  Alcotest.(check int) "count utf8 match" 3 (count ~pattern:"aうえ" "あいaうえ");
  Alcotest.(check int) "count utf8 negation" 3 (count ~pattern:"^あい" "あいaうえ");
  Alcotest.(check int)
    "count with range unicode" 3
    (count ~pattern:"A-Z" "abcABC");
  Alcotest.(check int)
    "count with full match" 5
    (count ~pattern:"あいうえお" "あいうえお");
  Alcotest.(check int) "count with empty pattern" 0 (count ~pattern:"" "abc");
  Alcotest.(check int) "count with empty string" 0 (count ~pattern:"abc" "")

let test_delete () =
  let open Stringx in
  Alcotest.(check string)
    "delete vowels" "hll"
    (delete ~pattern:"aeiou" "hello");
  Alcotest.(check string)
    "delete range a-k" "llo"
    (delete ~pattern:"a-k" "hello");
  Alcotest.(check string) "delete ^a-k" "he" (delete ~pattern:"^a-k" "hello");
  Alcotest.(check string) "delete unicode emoji" "🍏" (delete ~pattern:"🍎" "🍎🍏");
  Alcotest.(check string) "delete Japanese" "んにちは" (delete ~pattern:"こ" "こんにちは");
  Alcotest.(check string) "delete all" "" (delete ~pattern:"a-c" "abc");
  Alcotest.(check string) "delete nothing" "abc" (delete ~pattern:"xyz" "abc");
  Alcotest.(check string) "delete ^" "aeiou" (delete ~pattern:"^aeiou" "aeiou")

let test_length () =
  let open Stringx in
  Alcotest.(check int) "ascii" 5 (length "hello");
  Alcotest.(check int) "hiragana" 5 (length "こんにちは");
  Alcotest.(check int) "emoji" 3 (length "🍎🍏🍊");
  Alcotest.(check int) "empty" 0 (length "");
  Alcotest.(check int) "mixed ascii/emoji" 5 (length "a🍎b🍏c");
  Alcotest.(check int) "malformed utf8" 3 (length "a\xffb")

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
    "contains: foo in seafood" true
    (contains ~substr:"foo" "seafood");
  Alcotest.(check bool)
    "contains: bar in seafood" false
    (contains ~substr:"bar" "seafood");
  Alcotest.(check bool)
    "contains: empty in seafood" true
    (contains ~substr:"" "seafood");
  Alcotest.(check bool) "contains: empty in empty" true (contains ~substr:"" "");
  Alcotest.(check bool)
    "contains: non-empty in empty" false (contains ~substr:"a" "");
  Alcotest.(check bool)
    "contains: full match" true
    (contains ~substr:"abc" "abc");
  Alcotest.(check bool)
    "contains: partial match" true
    (contains ~substr:"bcd" "abcde");
  Alcotest.(check bool)
    "contains: unicode match" true
    (contains ~substr:"にち" "こんにちは");
  Alcotest.(check bool)
    "contains: unicode no match" false
    (contains ~substr:"さようなら" "こんにちは")

let test_contains_any () =
  let open Stringx in
  Alcotest.(check bool) "team/i" false (contains_any ~chars:"i" "team");
  Alcotest.(check bool) "fail/ui" true (contains_any ~chars:"ui" "fail");
  Alcotest.(check bool) "ure/ui" true (contains_any ~chars:"ui" "ure");
  Alcotest.(check bool) "failure/ui" true (contains_any ~chars:"ui" "failure");
  Alcotest.(check bool) "foo/empty" false (contains_any ~chars:"" "foo");
  Alcotest.(check bool) "empty/empty" false (contains_any ~chars:"" "");
  Alcotest.(check bool) "unicode/emoji" true (contains_any ~chars:"🍏" "🍎🍏🍊");
  Alcotest.(check bool)
    "unicode/no match" false
    (contains_any ~chars:"さよ" "こんにちは");
  Alcotest.(check bool) "unicode/match" true (contains_any ~chars:"ちに" "こんにちは")

let test_has_prefix () =
  let open Stringx in
  Alcotest.(check bool) "ascii: Go" true (has_prefix ~prefix:"Go" "Gopher");
  Alcotest.(check bool) "ascii: C" false (has_prefix ~prefix:"C" "Gopher");
  Alcotest.(check bool) "ascii: empty" true (has_prefix ~prefix:"" "Gopher");
  Alcotest.(check bool) "empty: empty" true (has_prefix ~prefix:"" "");
  Alcotest.(check bool) "empty: non-empty" false (has_prefix ~prefix:"a" "");
  Alcotest.(check bool) "full match" true (has_prefix ~prefix:"abc" "abc");
  Alcotest.(check bool) "partial match" true (has_prefix ~prefix:"abc" "abcde");
  Alcotest.(check bool) "unicode match" true (has_prefix ~prefix:"こん" "こんにちは");
  Alcotest.(check bool)
    "unicode no match" false
    (has_prefix ~prefix:"さよ" "こんにちは");
  Alcotest.(check bool) "emoji match" true (has_prefix ~prefix:"🍎" "🍎🍏🍊");
  Alcotest.(check bool) "emoji no match" false (has_prefix ~prefix:"🍊" "🍎🍏🍊")

let test_has_suffix () =
  let open Stringx in
  Alcotest.(check bool) "ascii: go" true (has_suffix ~suffix:"go" "Amigo");
  Alcotest.(check bool) "ascii: O" false (has_suffix ~suffix:"O" "Amigo");
  Alcotest.(check bool) "ascii: Ami" false (has_suffix ~suffix:"Ami" "Amigo");
  Alcotest.(check bool) "ascii: empty" true (has_suffix ~suffix:"" "Amigo");
  Alcotest.(check bool) "empty: empty" true (has_suffix ~suffix:"" "");
  Alcotest.(check bool) "empty: non-empty" false (has_suffix ~suffix:"a" "");
  Alcotest.(check bool) "full match" true (has_suffix ~suffix:"abc" "abc");
  Alcotest.(check bool) "partial match" true (has_suffix ~suffix:"cde" "abcde");
  Alcotest.(check bool) "unicode match" true (has_suffix ~suffix:"ちは" "こんにちは");
  Alcotest.(check bool)
    "unicode no match" false
    (has_suffix ~suffix:"さよ" "こんにちは");
  Alcotest.(check bool) "emoji match" true (has_suffix ~suffix:"🍊" "🍎🍏🍊");
  Alcotest.(check bool) "emoji no match" false (has_suffix ~suffix:"🍎" "🍎🍏🍊")

let test_count_substring () =
  let open Stringx in
  Alcotest.(check int) "cheese/e" 3 (count_substring ~substr:"e" "cheese");
  Alcotest.(check int) "five/empty" 5 (count_substring ~substr:"" "five");
  Alcotest.(check int) "banana/na" 2 (count_substring ~substr:"na" "banana");
  Alcotest.(check int) "aaaaa/aa" 2 (count_substring ~substr:"aa" "aaaaa");
  Alcotest.(check int) "empty/empty" 1 (count_substring ~substr:"" "");
  Alcotest.(check int) "empty/a" 0 (count_substring ~substr:"a" "");
  Alcotest.(check int) "abc/abc" 1 (count_substring ~substr:"abc" "abc");
  Alcotest.(check int) "abc/abcd" 0 (count_substring ~substr:"abcd" "abc");
  Alcotest.(check int) "unicode/に" 1 (count_substring ~substr:"に" "こんにちは");
  Alcotest.(check int) "unicode/ん" 1 (count_substring ~substr:"ん" "こんにちは");
  Alcotest.(check int) "unicode/empty" 6 (count_substring ~substr:"" "こんにちは");
  Alcotest.(check int) "emoji/🍏" 1 (count_substring ~substr:"🍏" "🍎🍏🍊");
  Alcotest.(check int) "emoji/🍎🍏" 1 (count_substring ~substr:"🍎🍏" "🍎🍏🍊")

let test_equal_fold () =
  let open Stringx in
  Alcotest.(check bool) "Go/go" true (equal_fold ~other:"go" "Go");
  Alcotest.(check bool) "AB/ab" true (equal_fold ~other:"ab" "AB");
  Alcotest.(check bool) "ß/ss" false (equal_fold ~other:"ss" "ß");
  Alcotest.(check bool) "ascii/ASCII" true (equal_fold ~other:"ascii" "ASCII");
  Alcotest.(check bool) "hiragana" false (equal_fold ~other:"ア" "あ");
  Alcotest.(check bool) "emoji" false (equal_fold ~other:"🍏" "🍎");
  Alcotest.(check bool) "empty" true (equal_fold ~other:"" "");
  Alcotest.(check bool) "mixed" false (equal_fold ~other:"Go!" "Go")

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
    (fields_func ~f "  foo1;bar2,baz3...");
  Alcotest.(check (list string))
    "fields_func only sep" [] (fields_func ~f ";;;;");
  Alcotest.(check (list string)) "fields_func empty" [] (fields_func ~f "");
  Alcotest.(check (list string))
    "fields_func single" [ "abc123" ] (fields_func ~f "abc123");
  Alcotest.(check (list string))
    "fields_func unicode"
    [ "こんにちは"; "123" ]
    (fields_func ~f:(fun u -> Uchar.to_int u = 0x2c) "こんにちは,123");
  Alcotest.(check (list string))
    "fields_func emoji" [ "🍎🍏"; "🍊" ]
    (fields_func ~f:(fun u -> Uchar.to_int u = 0x2c) "🍎🍏,🍊")

let test_index () =
  let open Stringx in
  Alcotest.(check int) "chicken/ken" 4 (index ~substr:"ken" "chicken");
  Alcotest.(check int) "chicken/dmr" (-1) (index ~substr:"dmr" "chicken");
  Alcotest.(check int) "abc/empty" 0 (index ~substr:"" "abc");
  Alcotest.(check int) "empty/empty" 0 (index ~substr:"" "");
  Alcotest.(check int) "empty/a" (-1) (index ~substr:"a" "");
  Alcotest.(check int) "abc/abc" 0 (index ~substr:"abc" "abc");
  Alcotest.(check int) "abc/abcd" (-1) (index ~substr:"abcd" "abc")

let test_repeat () =
  let open Stringx in
  Alcotest.(check string) "repeat na 2" "nana" (repeat ~count:2 "na");
  Alcotest.(check string) "repeat 🍎 3" "🍎🍎🍎" (repeat ~count:3 "🍎");
  Alcotest.(check string) "repeat empty 5" "" (repeat ~count:5 "");
  Alcotest.(check string) "repeat a 0" "" (repeat ~count:0 "a");
  Alcotest.check_raises "repeat negative"
    (Invalid_argument "repeat: negative count") (fun () ->
      ignore (repeat ~count:(-1) "abc"))

let test_join () =
  let open Stringx in
  Alcotest.(check string)
    "join basic" "foo, bar, baz"
    (join ~sep:", " [ "foo"; "bar"; "baz" ]);
  Alcotest.(check string) "join empty list" "" (join ~sep:", " []);
  Alcotest.(check string) "join single" "a" (join ~sep:", " [ "a" ]);
  Alcotest.(check string)
    "join unicode" "🍎-🍏-🍊"
    (join ~sep:"-" [ "🍎"; "🍏"; "🍊" ]);
  Alcotest.(check string)
    "join empty sep" "foobarbaz"
    (join ~sep:"" [ "foo"; "bar"; "baz" ]);
  Alcotest.(check string) "join all empty" "" (join ~sep:"" [])

let test_trim () =
  let open Stringx in
  Alcotest.(check string)
    "ascii trim" "Hello, Camels"
    (trim ~cutset:"!¡" "¡¡¡Hello, Camels!!!");
  Alcotest.(check string) "trim nothing" "hello" (trim ~cutset:"" "hello");
  Alcotest.(check string) "trim all" "" (trim ~cutset:"a" "aaa");
  Alcotest.(check string) "trim unicode" "b" (trim ~cutset:"あ" "ああbあ");
  Alcotest.(check string) "trim both sides" "b" (trim ~cutset:"xy" "xybxy");
  Alcotest.(check string) "trim only leading" "abc" (trim ~cutset:"!" "!!abc");
  Alcotest.(check string) "trim only trailing" "abc" (trim ~cutset:"!" "abc!!");
  Alcotest.(check string) "trim empty string" "" (trim ~cutset:"!" "");
  Alcotest.(check string) "trim emoji" "b" (trim ~cutset:"🍎" "🍎b🍎")

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
    (trim_func ~f "¡¡¡Hello, Camels!!!");
  Alcotest.(check string)
    "trim_func nothing" "hello"
    (trim_func ~f:(fun _ -> false) "hello");
  Alcotest.(check string)
    "trim_func all" ""
    (trim_func ~f:(fun _ -> true) "aaa");
  Alcotest.(check string)
    "trim_func unicode" "b"
    (trim_func ~f:(fun u -> Uchar.to_int u = 0x3042) "ああbあ");
  Alcotest.(check string)
    "trim_func emoji" "b"
    (trim_func ~f:(fun u -> Uchar.to_int u = 0x1F34E) "🍎b🍎");
  Alcotest.(check string) "trim_func empty" "" (trim_func ~f "")

let test_trim_left () =
  let open Stringx in
  Alcotest.(check string)
    "ascii trim_left" "Hello, Camels!!!"
    (trim_left ~cutset:"!¡" "¡¡¡Hello, Camels!!!");
  Alcotest.(check string)
    "trim_left nothing" "hello"
    (trim_left ~cutset:"" "hello");
  Alcotest.(check string) "trim_left all" "" (trim_left ~cutset:"a" "aaa");
  Alcotest.(check string)
    "trim_left unicode" "bあ"
    (trim_left ~cutset:"あ" "ああbあ");
  Alcotest.(check string)
    "trim_left both sides" "bxy"
    (trim_left ~cutset:"xy" "xybxy");
  Alcotest.(check string)
    "trim_left only leading" "abc"
    (trim_left ~cutset:"!" "!!abc");
  Alcotest.(check string)
    "trim_left only trailing" "abc!!"
    (trim_left ~cutset:"!" "abc!!");
  Alcotest.(check string) "trim_left empty string" "" (trim_left ~cutset:"!" "");
  Alcotest.(check string) "trim_left emoji" "b🍎" (trim_left ~cutset:"🍎" "🍎b🍎")

let test_trim_left_func () =
  let open Stringx in
  let f u = not (is_letter_or_number u) in
  Alcotest.(check string)
    "trim_left_func ascii/unicode" "Hello, Camels!!!"
    (trim_left_func ~f "¡¡¡Hello, Camels!!!");
  Alcotest.(check string)
    "trim_left_func nothing" "hello"
    (trim_left_func ~f:(fun _ -> false) "hello");
  Alcotest.(check string)
    "trim_left_func all" ""
    (trim_left_func ~f:(fun _ -> true) "aaa");
  Alcotest.(check string)
    "trim_left_func unicode" "bあ"
    (trim_left_func ~f:(fun u -> Uchar.to_int u = 0x3042) "ああbあ");
  Alcotest.(check string)
    "trim_left_func emoji" "b🍎"
    (trim_left_func ~f:(fun u -> Uchar.to_int u = 0x1F34E) "🍎b🍎");
  Alcotest.(check string) "trim_left_func empty" "" (trim_left_func ~f "")

let test_trim_right () =
  let open Stringx in
  Alcotest.(check string)
    "ascii trim_right" "¡¡¡Hello, Camels"
    (trim_right ~cutset:"!¡" "¡¡¡Hello, Camels!!!");
  Alcotest.(check string)
    "trim_right nothing" "hello"
    (trim_right ~cutset:"" "hello");
  Alcotest.(check string) "trim_right all" "" (trim_right ~cutset:"a" "aaa");
  Alcotest.(check string)
    "trim_right unicode" "ああb"
    (trim_right ~cutset:"あ" "ああbあ");
  Alcotest.(check string)
    "trim_right both sides" "xyb"
    (trim_right ~cutset:"xy" "xybxy");
  Alcotest.(check string)
    "trim_right only trailing" "abc"
    (trim_right ~cutset:"!" "abc!!");
  Alcotest.(check string)
    "trim_right only leading" "!!abc"
    (trim_right ~cutset:"!" "!!abc");
  Alcotest.(check string)
    "trim_right empty string" ""
    (trim_right ~cutset:"!" "");
  Alcotest.(check string) "trim_right emoji" "🍎b" (trim_right ~cutset:"🍎" "🍎b🍎")

let test_trim_right_func () =
  let open Stringx in
  let f u = not (is_letter_or_number u) in
  Alcotest.(check string)
    "trim_right_func ascii/unicode" "¡¡¡Hello, Camels"
    (trim_right_func ~f "¡¡¡Hello, Camels!!!");
  Alcotest.(check string)
    "trim_right_func nothing" "hello"
    (trim_right_func ~f:(fun _ -> false) "hello");
  Alcotest.(check string)
    "trim_right_func all" ""
    (trim_right_func ~f:(fun _ -> true) "aaa");
  Alcotest.(check string)
    "trim_right_func unicode" "ああb"
    (trim_right_func ~f:(fun u -> Uchar.to_int u = 0x3042) "ああbあ");
  Alcotest.(check string)
    "trim_right_func emoji" "🍎b"
    (trim_right_func ~f:(fun u -> Uchar.to_int u = 0x1F34E) "🍎b🍎");
  Alcotest.(check string) "trim_right_func empty" "" (trim_right_func ~f "")

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
    (trim_suffix ~suffix:", Camels!!!" "¡¡¡Hello, Camels!!!");
  Alcotest.(check string)
    "ascii: , Marmots!!!" "¡¡¡Hello, Camels!!!"
    (trim_suffix ~suffix:", Marmots!!!" "¡¡¡Hello, Camels!!!");
  Alcotest.(check string) "empty suffix" "abc" (trim_suffix ~suffix:"" "abc");
  Alcotest.(check string) "empty string" "" (trim_suffix ~suffix:"abc" "");
  Alcotest.(check string) "full match" "" (trim_suffix ~suffix:"abc" "abc");
  Alcotest.(check string)
    "partial match" "abc"
    (trim_suffix ~suffix:"de" "abcde");
  Alcotest.(check string)
    "unicode match" "こんに"
    (trim_suffix ~suffix:"ちは" "こんにちは");
  Alcotest.(check string)
    "unicode no match" "こんにちは"
    (trim_suffix ~suffix:"さよ" "こんにちは");
  Alcotest.(check string) "emoji match" "🍎🍏" (trim_suffix ~suffix:"🍊" "🍎🍏🍊");
  Alcotest.(check string) "emoji no match" "🍎🍏🍊" (trim_suffix ~suffix:"🍎" "🍎🍏🍊")

let test_to_lower () =
  let open Stringx in
  Alcotest.(check string) "ascii" "camel" (to_lower "Camel");
  Alcotest.(check string) "all upper" "camel" (to_lower "CAMEL");
  Alcotest.(check string) "all lower" "camel" (to_lower "camel");
  Alcotest.(check string) "mixed" "camel123" (to_lower "CaMeL123");
  Alcotest.(check string) "unicode" "こんにちは" (to_lower "こんにちは");
  Alcotest.(check string) "emoji" "🍎" (to_lower "🍎");
  Alcotest.(check string) "empty" "" (to_lower "");
  Alcotest.(check string) "ascii+unicode" "camelこんにちは" (to_lower "CAMELこんにちは")

let test_to_title () =
  let open Stringx in
  Alcotest.(check string)
    "ascii phrase" "HER ROYAL HIGHNESS"
    (to_title "her royal highness");
  Alcotest.(check string) "ascii loud" "LOUD NOISES" (to_title "loud noises");
  Alcotest.(check string) "ascii already upper" "LOUD" (to_title "LOUD");
  Alcotest.(check string) "ascii mixed" "CAMEL" (to_title "Camel");
  Alcotest.(check string) "unicode cyrillic" "брат" (to_title "брат");
  Alcotest.(check string) "hiragana" "こんにちは" (to_title "こんにちは");
  Alcotest.(check string) "emoji" "🍎🍏" (to_title "🍎🍏");
  Alcotest.(check string) "empty" "" (to_title "");
  Alcotest.(check string) "ascii+unicode" "CAMELこんにちは" (to_title "Camelこんにちは")

let test_to_upper () =
  let open Stringx in
  Alcotest.(check string) "ascii" "CAMEL" (to_upper "Camel");
  Alcotest.(check string) "all lower" "CAMEL" (to_upper "camel");
  Alcotest.(check string) "all upper" "CAMEL" (to_upper "CAMEL");
  Alcotest.(check string) "mixed" "CAMEL123" (to_upper "CaMeL123");
  Alcotest.(check string) "unicode" "こんにちは" (to_upper "こんにちは");
  Alcotest.(check string) "emoji" "🍎" (to_upper "🍎");
  Alcotest.(check string) "empty" "" (to_upper "");
  Alcotest.(check string) "ascii+unicode" "CAMELこんにちは" (to_upper "Camelこんにちは")

let test_to_camel_case () =
  let open Stringx in
  Alcotest.(check string) "some_words" "someWords" (to_camel_case "some_words");
  Alcotest.(check string)
    "http_server" "httpServer"
    (to_camel_case "http_server");
  Alcotest.(check string) "no_https" "noHttps" (to_camel_case "no_https");
  Alcotest.(check string)
    "_complex__case_" "_complexCase_"
    (to_camel_case "_complex__case_");
  Alcotest.(check string) "some words" "someWords" (to_camel_case "some words");
  Alcotest.(check string)
    "OCAML_IS_GREAT" "ocamlIsGreat"
    (to_camel_case "OCAML_IS_GREAT");
  Alcotest.(check string)
    "alreadyCamel" "alreadycamel"
    (to_camel_case "alreadyCamel");
  Alcotest.(check string) "empty" "" (to_camel_case "");
  Alcotest.(check string) "single word" "word" (to_camel_case "word");
  Alcotest.(check string) "hyphen" "fooBarBaz" (to_camel_case "foo-BarBaz")

let test_to_kebab_case () =
  let open Stringx in
  Alcotest.(check string) "FirstName" "first-name" (to_kebab_case "FirstName");
  Alcotest.(check string)
    "HTTPServer" "http-server"
    (to_kebab_case "HTTPServer");
  Alcotest.(check string) "NoHTTPS" "no-https" (to_kebab_case "NoHTTPS");
  Alcotest.(check string) "GO_PATH" "go-path" (to_kebab_case "GO_PATH");
  Alcotest.(check string) "GO PATH" "go-path" (to_kebab_case "GO PATH");
  Alcotest.(check string) "GO-PATH" "go-path" (to_kebab_case "GO-PATH");
  Alcotest.(check string) "http2xx" "http-2xx" (to_kebab_case "http2xx");
  Alcotest.(check string) "HTTP20xOK" "http-20x-ok" (to_kebab_case "HTTP20xOK");
  Alcotest.(check string)
    "Duration2m3s" "duration-2m-3s"
    (to_kebab_case "Duration2m3s");
  Alcotest.(check string)
    "Bld4Floor3rd" "bld4-floor-3rd"
    (to_kebab_case "Bld4Floor3rd");
  Alcotest.(check string) "empty" "" (to_kebab_case "");
  Alcotest.(check string) "single lower" "abc" (to_kebab_case "abc");
  Alcotest.(check string) "single upper" "a" (to_kebab_case "A");
  Alcotest.(check string) "hyphens" "foo-bar-baz" (to_kebab_case "FooBarBaz")

let test_to_pascal_case () =
  let open Stringx in
  Alcotest.(check string) "some_words" "SomeWords" (to_pascal_case "some_words");
  Alcotest.(check string)
    "_complex__case_" "_ComplexCase_"
    (to_pascal_case "_complex__case_");
  Alcotest.(check string)
    "OCAML_IS_GREAT" "OcamlIsGreat"
    (to_pascal_case "OCAML_IS_GREAT");
  Alcotest.(check string)
    "alreadyPascal" "AlreadyPascal"
    (to_pascal_case "alreadyPascal");
  Alcotest.(check string) "foo-BarBaz" "FooBarBaz" (to_pascal_case "foo-BarBaz");
  Alcotest.(check string) "word" "Word" (to_pascal_case "word");
  Alcotest.(check string) "empty" "" (to_pascal_case "");
  Alcotest.(check string) "spaces" "SomeWords" (to_pascal_case "some words");
  Alcotest.(check string)
    "http_server" "HttpServer"
    (to_pascal_case "http_server");
  Alcotest.(check string) "no_https" "NoHttps" (to_pascal_case "no_https")

let test_to_snake_case () =
  let open Stringx in
  Alcotest.(check string) "FirstName" "first_name" (to_snake_case "FirstName");
  Alcotest.(check string)
    "HTTPServer" "http_server"
    (to_snake_case "HTTPServer");
  Alcotest.(check string) "NoHTTPS" "no_https" (to_snake_case "NoHTTPS");
  Alcotest.(check string) "GO_PATH" "go_path" (to_snake_case "GO_PATH");
  Alcotest.(check string) "GO PATH" "go_path" (to_snake_case "GO PATH");
  Alcotest.(check string) "GO-PATH" "go_path" (to_snake_case "GO-PATH");
  Alcotest.(check string) "http2xx" "http_2xx" (to_snake_case "http2xx");
  Alcotest.(check string) "HTTP20xOK" "http_20x_ok" (to_snake_case "HTTP20xOK");
  Alcotest.(check string)
    "Duration2m3s" "duration_2m3s"
    (to_snake_case "Duration2m3s");
  Alcotest.(check string)
    "Bld4Floor3rd" "bld4_floor_3rd"
    (to_snake_case "Bld4Floor3rd");
  Alcotest.(check string) "empty" "" (to_snake_case "");
  Alcotest.(check string) "single lower" "abc" (to_snake_case "abc");
  Alcotest.(check string) "single upper" "a" (to_snake_case "A");
  Alcotest.(check string) "hyphens" "foo_bar_baz" (to_snake_case "FooBarBaz")

let test_map () =
  let open Stringx in
  let rot13 u =
    let c = Uchar.to_int u in
    let d =
      if c >= int_of_char 'A' && c <= int_of_char 'Z' then
        int_of_char 'A' + ((c - int_of_char 'A' + 13) mod 26)
      else if c >= int_of_char 'a' && c <= int_of_char 'z' then
        int_of_char 'a' + ((c - int_of_char 'a' + 13) mod 26)
      else c
    in
    Uchar.of_int d
  in

  Alcotest.(check string)
    "rot13" "'Gjnf oevyyvt naq gur fyvgul pnzry..."
    (map ~f:rot13 "'Twas brillig and the slithy camel...")

let test_filter_map () =
  let open Stringx in
  (* Test dropping vowels *)
  let drop_vowel u =
    match Uchar.to_int u with
    | c
      when List.mem c
             [
               Char.code 'a';
               Char.code 'e';
               Char.code 'i';
               Char.code 'o';
               Char.code 'u';
             ] ->
        None
    | _ -> Some u
  in
  Alcotest.(check string) "drop vowels" "hll" (filter_map ~f:drop_vowel "hello");

  (* Test shifting lowercase letters by one, leaving others unchanged *)
  let shift_alpha u =
    match Uchar.to_int u with
    | c when Char.code 'a' <= c && c <= Char.code 'z' ->
        Some (Uchar.of_int (Char.code 'a' + ((c - Char.code 'a' + 1) mod 26)))
    | _ -> Some u
  in
  Alcotest.(check string)
    "shift next (letters)" "ifmmp"
    (filter_map ~f:shift_alpha "hello");

  (* Test identity mapping: no change *)
  let identity u = Some u in
  Alcotest.(check string) "identity" "こんにちは😊" (filter_map ~f:identity "こんにちは😊")

let test_iter () =
  let open Stringx in
  (* Test that iter visits each code point in order *)
  let buf = Buffer.create 16 in
  iter ~f:(fun u -> Buffer.add_utf_8_uchar buf u) "abcXYZ😊";
  Alcotest.(check string)
    "iter reconstructs original" "abcXYZ😊" (Buffer.contents buf)

let test_fold_count () =
  let open Stringx in
  (* Test counting code points *)
  let count acc _ = acc + 1 in
  Alcotest.(check int) "count code points" 5 (fold ~f:count ~init:0 "hello")

let test_fold_sum () =
  let open Stringx in
  (* Test summing Unicode code point values *)
  let sum acc u = acc + Uchar.to_int u in
  Alcotest.(check int) "sum code point ints" 198 (fold ~f:sum ~init:0 "ABC")

let test_expand_tabs () =
  let open Stringx in
  Alcotest.(check string)
    "expand_tabs basic" "a   bc  def ghij    k"
    (expand_tabs ~tab_size:4 "a\tbc\tdef\tghij\tk");
  Alcotest.(check string)
    "expand_tabs newline" "abcdefg hij\nk   l"
    (expand_tabs ~tab_size:4 "abcdefg\thij\nk\tl");
  Alcotest.(check string)
    "expand_tabs CJK" "z中 文  w"
    (expand_tabs ~tab_size:4 "z中\t文\tw");
  Alcotest.check_raises "expand_tabs tab_size <= 0"
    (Invalid_argument "expand_tabs: tab_size must be > 0") (fun () ->
      ignore (expand_tabs ~tab_size:0 "abc\tdef"))

let test_first_rune_to_lower () =
  let open Stringx in
  Alcotest.(check string)
    "ascii upper" "camelCase"
    (first_rune_to_lower "CamelCase");
  Alcotest.(check string)
    "ascii lower" "camelCase"
    (first_rune_to_lower "camelCase");
  Alcotest.(check string) "all upper" "cAMEL" (first_rune_to_lower "CAMEL");
  Alcotest.(check string) "single upper" "c" (first_rune_to_lower "C");
  Alcotest.(check string) "single lower" "c" (first_rune_to_lower "c");
  Alcotest.(check string) "unicode" "こんにちは" (first_rune_to_lower "こんにちは");
  Alcotest.(check string) "empty" "" (first_rune_to_lower "")

let test_first_rune_to_upper () =
  let open Stringx in
  Alcotest.(check string)
    "ascii lower" "CamelCase"
    (first_rune_to_upper "camelCase");
  Alcotest.(check string)
    "ascii upper" "CamelCase"
    (first_rune_to_upper "CamelCase");
  Alcotest.(check string) "all lower" "Camel" (first_rune_to_upper "camel");
  Alcotest.(check string) "all upper" "CAMEL" (first_rune_to_upper "CAMEL");
  Alcotest.(check string) "single lower" "C" (first_rune_to_upper "c");
  Alcotest.(check string) "single upper" "C" (first_rune_to_upper "C");
  Alcotest.(check string) "unicode" "こんにちは" (first_rune_to_upper "こんにちは");
  Alcotest.(check string) "empty" "" (first_rune_to_upper "")

let test_insert () =
  let open Stringx in
  Alcotest.(check string)
    "insert ascii middle" "CamelSuperCase"
    (insert ~src:"Super" ~index:5 "CamelCase");
  Alcotest.(check string)
    "insert ascii start" "SuperCamelCase"
    (insert ~src:"Super" ~index:0 "CamelCase");
  Alcotest.(check string)
    "insert ascii end" "CamelCaseSuper"
    (insert ~src:"Super" ~index:9 "CamelCase");
  Alcotest.(check string)
    "insert unicode" "こん世界にちは"
    (insert ~src:"世界" ~index:2 "こんにちは");
  Alcotest.(check string)
    "insert empty src" "CamelCase"
    (insert ~src:"" ~index:5 "CamelCase");
  Alcotest.(check string)
    "insert empty dst" "Super"
    (insert ~src:"Super" ~index:0 "");
  Alcotest.check_raises "insert negative index"
    (Invalid_argument "insert: index out of range") (fun () ->
      ignore (insert ~src:"Super" ~index:(-1) "CamelCase"));
  Alcotest.check_raises "insert index too large"
    (Invalid_argument "insert: index out of range") (fun () ->
      ignore (insert ~src:"Super" ~index:10 "CamelCase"))

let test_last_partition () =
  let open Stringx in
  Alcotest.(check (triple string string string))
    "last_partition: found" ("hel", "l", "o")
    (last_partition ~sep:"l" "hello");
  Alcotest.(check (triple string string string))
    "last_partition: not found" ("", "", "hello")
    (last_partition ~sep:"x" "hello");
  Alcotest.(check (triple string string string))
    "last_partition: sep at start" ("", "h", "ello")
    (last_partition ~sep:"h" "hello");
  Alcotest.(check (triple string string string))
    "last_partition: sep at end" ("hell", "o", "")
    (last_partition ~sep:"o" "hello")

let test_left_justify () =
  let open Stringx in
  Alcotest.(check string)
    "shorter than length" "hello"
    (left_justify ~width:4 ~pad:" " "hello");
  Alcotest.(check string)
    "longer than length" "hello"
    (left_justify ~width:3 ~pad:" " "hello");
  Alcotest.(check string)
    "pad with spaces" "hello     "
    (left_justify ~width:10 ~pad:" " "hello");
  Alcotest.(check string)
    "pad with multi" "hello12312"
    (left_justify ~width:10 ~pad:"123" "hello");
  Alcotest.(check string)
    "pad empty" "hello"
    (left_justify ~width:10 ~pad:"" "hello");
  Alcotest.(check string)
    "unicode pad" "helloあいあい"
    (left_justify ~width:9 ~pad:"あい" "hello");
  Alcotest.(check string)
    "unicode input" "こんにちは  "
    (left_justify ~width:7 ~pad:" " "こんにちは");
  Alcotest.(check string)
    "pad exact" "hello12345"
    (left_justify ~width:10 ~pad:"12345" "hello")

let test_partition () =
  let open Stringx in
  Alcotest.(check (triple string string string))
    "partition: found" ("he", "l", "lo")
    (partition ~sep:"l" "hello");
  Alcotest.(check (triple string string string))
    "partition: not found" ("hello", "", "")
    (partition ~sep:"x" "hello");
  Alcotest.(check (triple string string string))
    "partition: sep at start" ("", "h", "ello")
    (partition ~sep:"h" "hello");
  Alcotest.(check (triple string string string))
    "partition: sep at end" ("hell", "o", "")
    (partition ~sep:"o" "hello")

let test_right_justify () =
  let open Stringx in
  Alcotest.(check string)
    "shorter than length" "hello"
    (right_justify ~width:4 ~pad:" " "hello");
  Alcotest.(check string)
    "pad with spaces" "     hello"
    (right_justify ~width:10 ~pad:" " "hello");
  Alcotest.(check string)
    "pad with multi" "12312hello"
    (right_justify ~width:10 ~pad:"123" "hello");
  Alcotest.(check string)
    "pad empty" "hello"
    (right_justify ~width:10 ~pad:"" "hello");
  Alcotest.(check string)
    "unicode pad" "あいhello"
    (right_justify ~width:7 ~pad:"あい" "hello");
  Alcotest.(check string)
    "unicode input" "  こんにちは"
    (right_justify ~width:7 ~pad:" " "こんにちは");
  Alcotest.(check string)
    "pad exact" "12345hello"
    (right_justify ~width:10 ~pad:"12345" "hello")

let test_rune_width () =
  let open Stringx in
  let u = Uchar.of_int in
  Alcotest.(check int) "ASCII" 1 (rune_width (u (Char.code 'a')));
  Alcotest.(check int) "Latin-1" 1 (rune_width (u 0x00E9));
  (* é *)
  Alcotest.(check int) "Hiragana" 2 (rune_width (u 0x3042));
  (* あ *)
  Alcotest.(check int) "Katakana" 2 (rune_width (u 0x30AB));
  (* カ *)
  Alcotest.(check int) "CJK Unified Ideograph" 2 (rune_width (u 0x4E2D));
  (* 中 *)
  Alcotest.(check int) "Hangul" 2 (rune_width (u 0xAC00));
  (* 가 *)
  Alcotest.(check int) "Fullwidth Latin" 2 (rune_width (u 0xFF21));
  (* Ａ *)
  Alcotest.(check int) "Emoji" 2 (rune_width (u 0x1F34E));
  (* 🍎 *)
  Alcotest.(check int) "Misc symbol" 1 (rune_width (u 0x2603))
(* ☃ *)

let test_scrub () =
  let open Stringx in
  Alcotest.(check string) "no invalid" "abc" (scrub ~repl:"?" "abc");
  Alcotest.(check string) "single invalid" "a?b" (scrub ~repl:"?" "a\xffb");
  Alcotest.(check string)
    "adjacent invalid" "a?b"
    (scrub ~repl:"?" "a\xff\xffb");
  Alcotest.(check string)
    "multiple invalid" "a?b?"
    (scrub ~repl:"?" "a\xffb\xff");
  Alcotest.(check string) "all invalid" "?" (scrub ~repl:"?" "\xff\xff");
  Alcotest.(check string) "empty" "" (scrub ~repl:"?" "");
  Alcotest.(check string) "custom repl" "aXb" (scrub ~repl:"X" "a\xffb");
  Alcotest.(check string) "unicode valid" "こんにちは" (scrub ~repl:"?" "こんにちは")

let test_shuffle () =
  let open Stringx in
  Random.init 42;
  (* deterministic for test *)
  let s = "Camel" in
  let shuffled = shuffle s in
  Alcotest.(check int) "same length" (String.length s) (String.length shuffled);
  Alcotest.(check (list char))
    "same code points"
    (List.sort compare (List.init (String.length s) (String.get s)))
    (List.sort compare
       (List.init (String.length shuffled) (String.get shuffled)));
  (* Unicode test *)
  let s2 = "こんにちは" in
  let shuffled2 = shuffle s2 in
  Alcotest.(check int)
    "unicode same length" (String.length s2) (String.length shuffled2);
  Alcotest.(check int)
    "unicode same rune count" (Stringx.length s2) (Stringx.length shuffled2);
  Alcotest.(check string) "empty" "" (shuffle "")

let test_shuffle_source () =
  let open Stringx in
  let rand = Random.State.make [| 42 |] in
  let s = "Camel" in
  let shuffled = shuffle_source ~rand s in
  Alcotest.(check int) "same length" (String.length s) (String.length shuffled);
  Alcotest.(check (list char))
    "same code points"
    (List.sort compare (List.init (String.length s) (String.get s)))
    (List.sort compare
       (List.init (String.length shuffled) (String.get shuffled)));
  (* Unicode test *)
  let rand2 = Random.State.make [| 123 |] in
  let s2 = "こんにちは" in
  let shuffled2 = shuffle_source ~rand:rand2 s2 in
  Alcotest.(check int)
    "unicode same length" (String.length s2) (String.length shuffled2);
  Alcotest.(check int)
    "unicode same rune count" (Stringx.length s2) (Stringx.length shuffled2);
  Alcotest.(check (list char))
    "unicode same code points"
    (List.sort compare (List.init (String.length s2) (String.get s2)))
    (List.sort compare
       (List.init (String.length shuffled2) (String.get shuffled2)));
  Alcotest.(check string) "empty" "" (shuffle_source ~rand "")

let test_slice () =
  let open Stringx in
  Alcotest.(check string)
    "ascii 0-5" "Camel"
    (slice ~start:0 ~end_:5 "CamelCase");
  Alcotest.(check string)
    "ascii 5--1" "Case"
    (slice ~start:5 ~end_:(-1) "CamelCase");
  Alcotest.(check string)
    "ascii 0--1" "CamelCase"
    (slice ~start:0 ~end_:(-1) "CamelCase");
  Alcotest.(check string) "ascii 2-4" "me" (slice ~start:2 ~end_:4 "CamelCase");
  Alcotest.(check string) "unicode 2-4" "にち" (slice ~start:2 ~end_:4 "こんにちは");
  Alcotest.(check string)
    "unicode 2--1" "にちは"
    (slice ~start:2 ~end_:(-1) "こんにちは");
  Alcotest.(check string) "emoji 1-2" "🍏" (slice ~start:1 ~end_:2 "🍎🍏🍊");
  Alcotest.(check string) "emoji 0-2" "🍎🍏" (slice ~start:0 ~end_:2 "🍎🍏🍊");
  Alcotest.(check string) "empty" "" (slice ~start:0 ~end_:(-1) "");
  Alcotest.check_raises "start < 0"
    (Invalid_argument "slice: start out of range") (fun () ->
      ignore (slice ~start:(-1) ~end_:2 "Camel"));
  Alcotest.check_raises "start > len"
    (Invalid_argument "slice: start out of range") (fun () ->
      ignore (slice ~start:6 ~end_:7 "Camel"));
  Alcotest.check_raises "end > len" (Invalid_argument "slice: end out of range")
    (fun () -> ignore (slice ~start:0 ~end_:10 "Camel"));
  Alcotest.check_raises "end < start" (Invalid_argument "slice: end < start")
    (fun () -> ignore (slice ~start:3 ~end_:2 "Camel"))

let test_squeeze () =
  let open Stringx in
  Alcotest.(check string) "no pattern" "helo" (squeeze ~pattern:"" "hello");
  Alcotest.(check string) "pattern m-z" "hello" (squeeze ~pattern:"m-z" "hello");
  Alcotest.(check string)
    "spaces" "hello world"
    (squeeze ~pattern:" " "hello   world");
  Alcotest.(check string) "all spaces" " " (squeeze ~pattern:"" "     ");
  Alcotest.(check string) "unicode" "こんにちは" (squeeze ~pattern:"" "こんににちは");
  Alcotest.(check string) "emoji" "🍎🍏🍊" (squeeze ~pattern:"" "🍎🍎🍏🍊🍊");
  Alcotest.(check string) "empty" "" (squeeze ~pattern:"" "");
  Alcotest.(check string) "single char" "a" (squeeze ~pattern:"" "a");
  Alcotest.(check string)
    "pattern subset" "heelo"
    (squeeze ~pattern:"l" "heelllo");
  Alcotest.(check string)
    "pattern negation" "helo"
    (squeeze ~pattern:"^e" "hello")

let () =
  run "stringx"
    [
      ( "UTF-8 distance tests",
        [ test_case "basic distances" `Quick test_distance ] );
      ("center tests", [ test_case "center basic" `Quick test_center ]);
      ("count tests", [ test_case "count basic" `Quick test_count ]);
      ("delete tests", [ test_case "delete basic" `Quick test_delete ]);
      ("length tests", [ test_case "length basic" `Quick test_length ]);
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
      ("to lower tests", [ test_case "to lower basic" `Quick test_to_lower ]);
      ("to title tests", [ test_case "to title basic" `Quick test_to_title ]);
      ("to upper tests", [ test_case "to upper basic" `Quick test_to_upper ]);
      ( "to camel case tests",
        [ test_case "to camel case basic" `Quick test_to_camel_case ] );
      ( "to kebab case tests",
        [ test_case "to kebab case basic" `Quick test_to_kebab_case ] );
      ( "to pascal case tests",
        [ test_case "to pascal case basic" `Quick test_to_pascal_case ] );
      ( "to snake case tests",
        [ test_case "to snake case basic" `Quick test_to_snake_case ] );
      ("map tests", [ test_case "map basic" `Quick test_map ]);
      ( "filter map tests",
        [ test_case "filter map basic" `Quick test_filter_map ] );
      ("iter tests", [ test_case "iter basic" `Quick test_iter ]);
      ( "fold count tests",
        [ test_case "fold count basic" `Quick test_fold_count ] );
      ("fold sum tests", [ test_case "fold sum basic" `Quick test_fold_sum ]);
      ( "expand tabs tests",
        [ test_case "expand tabs basic" `Quick test_expand_tabs ] );
      ( "first rune to lower tests",
        [
          test_case "first rune to lower basic" `Quick test_first_rune_to_lower;
        ] );
      ( "first rune to upper tests",
        [
          test_case "first rune to upper basic" `Quick test_first_rune_to_upper;
        ] );
      ("insert tests", [ test_case "insert basic" `Quick test_insert ]);
      ( "last partition tests",
        [ test_case "last partition basic" `Quick test_last_partition ] );
      ( "left justify tests",
        [ test_case "left justify basic" `Quick test_left_justify ] );
      ("partition tests", [ test_case "partition basic" `Quick test_partition ]);
      ( "right justify tests",
        [ test_case "right justify basic" `Quick test_right_justify ] );
      ( "rune width tests",
        [ test_case "rune width basic" `Quick test_rune_width ] );
      ("scrub tests", [ test_case "scrub basic" `Quick test_scrub ]);
      ("shuffle tests", [ test_case "shuffle basic" `Quick test_shuffle ]);
      ( "shuffle source tests",
        [ test_case "shuffle source basic" `Quick test_shuffle_source ] );
      ("slice tests", [ test_case "slice basic" `Quick test_slice ]);
      ("squeeze tests", [ test_case "squeeze basic" `Quick test_squeeze ]);
    ]
