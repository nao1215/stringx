module Levenshtein : sig
  val distance : string -> string -> int
  (** [distance s t] computes the Levenshtein (edit) distance between two UTF-8
      encoded strings.

      The Levenshtein distance is the minimum number of single-character edits
      (insertions, deletions, or substitutions) required to transform one string
      into another.

      This implementation is Unicode-aware and correctly handles multibyte
      characters such as Japanese, Chinese, emoji, and accented letters.

      Examples:
      - [distance "kitten" "sitting"] returns [3]
      - [distance "こんにちは" "こんばんは"] returns [2]
      - [distance "🍎" "🍏"] returns [1]

      Malformed UTF-8 sequences are replaced with ['?'] during decoding.

      @param s The first UTF-8 encoded string
      @param t The second UTF-8 encoded string
      @return The edit distance between [s] and [t] *)
end

val center : string -> int -> string -> string
(** [center s len pad] centers [s] in a string of length [len], padding with
    [pad]. If [s] is longer than [len], it is returned unchanged. Padding is
    inserted symmetrically. [pad] must be non-empty or it is ignored.

    This function is Unicode-aware and counts characters, not bytes. If [pad] is
    multibyte, it is repeated and truncated as needed.

    Examples:
    - [center "hello" 10 " "] returns ["  hello   "]
    - [center "abc" 7 "あ"] returns ["ああabcああ"]

    @param s The string to center (UTF-8)
    @param len The total length (in Unicode characters) of the result
    @param pad The padding string (UTF-8, non-empty)
    @return The centered string *)

val count : string -> string -> int
(** [count str pattern] counts how many Unicode characters in [str] match
    [pattern].

    The [pattern] supports:
    - character sets: e.g., "aeiou"
    - ranges: e.g., "a-k", "あ-ん"
    - negation with ^: e.g., "^a-k", "^0-9"

    This function is Unicode-aware and handles UTF-8 properly.

    Examples:
    - [count "hello" "aeiou"] returns [2]
    - [count "abc123" "^a-z"] returns [3]
    - [count "こんにちは" "あ-ん"] returns [5]

    @param str The input string (UTF-8)
    @param pattern The character pattern (see above)
    @return The number of matching characters *)

val delete : string -> string -> string
(** [delete str pattern] removes all Unicode characters in [str] that match
    [pattern].

    The [pattern] supports:
    - character sets: e.g., "aeiou"
    - ranges: e.g., "a-k", "あ-ん"
    - negation with ^: e.g., "^a-k", "^0-9"

    This function is Unicode-aware and handles UTF-8 properly.

    Examples:
    - [delete "hello" "aeiou"] returns ["hll"]
    - [delete "こんにちは" "こ"] returns ["んにちは"]
    - [delete "abc123" "^a-z"] returns ["abc"]

    @param str The input string (UTF-8)
    @param pattern The character pattern (see above)
    @return The string with matched characters removed *)

val len : string -> int
(** [len str] returns the number of Unicode code points (runes) in UTF-8 string
    [str].

    This function is Unicode-aware and counts characters, not bytes.

    Examples:
    - [len "hello"] returns [5]
    - [len "こんにちは"] returns [5]
    - [len "🍎🍏🍊"] returns [3]

    @param str The input string (UTF-8)
    @return The number of Unicode code points in [str] *)

val reverse : string -> string
(** [reverse s] reverses a UTF-8 encoded string [s].

    This function is Unicode-aware and reverses by code points, not bytes.

    Examples:
    - [reverse "hello"] returns ["olleh"]
    - [reverse "こんにちは"] returns ["はちにんこ"]
    - [reverse "🍎🍏🍊"] returns ["🍊🍏🍎"]

    @param s The input string (UTF-8)
    @return The reversed string *)

val contains : string -> string -> bool
(** [contains s substr] reports whether [substr] is within [s].

    Returns [true] if [substr] is the empty string, or if [substr] occurs
    anywhere in [s]. Returns [false] otherwise.

    This function is Unicode-agnostic and operates on bytes, not code points.

    Examples:
    - [contains "seafood" "foo"] returns [true]
    - [contains "seafood" "bar"] returns [false]
    - [contains "seafood" ""] returns [true]
    - [contains "" ""] returns [true]

    @param s The input string
    @param substr The substring to search for
    @return [true] if [substr] is found in [s], [false] otherwise *)

val contains_any : string -> string -> bool
(** [contains_any s chars] reports whether any Unicode code points in [chars]
    are within [s].

    Returns [false] if [chars] is empty. Unicode-aware and compares by code
    points.

    Examples:
    - [contains_any "team" "i"] returns [false]
    - [contains_any "fail" "ui"] returns [true]
    - [contains_any "ure" "ui"] returns [true]
    - [contains_any "failure" "ui"] returns [true]
    - [contains_any "foo" ""] returns [false]
    - [contains_any "" ""] returns [false]

    @param s The input string (UTF-8)
    @param chars The set of Unicode code points to search for (UTF-8)
    @return
      [true] if any code point in [chars] is found in [s], [false] otherwise *)

val has_prefix : string -> string -> bool
(** [has_prefix s prefix] reports whether the string [s] begins with [prefix].

    Returns [true] if [prefix] is the empty string, or if [s] starts with
    [prefix]. Returns [false] otherwise.

    This function is Unicode-agnostic and operates on bytes, not code points.

    Examples:
    - [has_prefix "Gopher" "Go"] returns [true]
    - [has_prefix "Gopher" "C"] returns [false]
    - [has_prefix "Gopher" ""] returns [true]

    @param s The input string
    @param prefix The prefix to test
    @return [true] if [s] starts with [prefix], [false] otherwise *)

val has_suffix : string -> string -> bool
(** [has_suffix s suffix] reports whether the string [s] ends with [suffix].

    Returns [true] if [suffix] is the empty string, or if [s] ends with
    [suffix]. Returns [false] otherwise.

    This function is Unicode-agnostic and operates on bytes, not code points.

    Examples:
    - [has_suffix "Amigo" "go"] returns [true]
    - [has_suffix "Amigo" "O"] returns [false]
    - [has_suffix "Amigo" "Ami"] returns [false]
    - [has_suffix "Amigo" ""] returns [true]

    @param s The input string
    @param suffix The suffix to test
    @return [true] if [s] ends with [suffix], [false] otherwise *)

val count_substring : string -> string -> int
(** [count_substring s substr] counts the number of non-overlapping instances of
    [substr] in [s].

    If [substr] is the empty string, returns 1 + the number of Unicode code
    points in [s].

    This function is Unicode-agnostic and operates on bytes, not code points.

    Examples:
    - [count_substring "cheese" "e"] returns [3]
    - [count_substring "five" ""] returns [5]
    - [count_substring "banana" "na"] returns [2]
    - [count_substring "aaaaa" "aa"] returns [2]
    - [count_substring "" ""] returns [1]
    - [count_substring "" "a"] returns [0]

    @param s The input string
    @param substr The substring to count
    @return The number of non-overlapping instances of [substr] in [s] *)

val equal_fold : string -> string -> bool
(** [equal_fold s t] reports whether [s] and [t], interpreted as UTF-8 strings,
    are equal under simple Unicode case-folding (ASCII only).

    This is a simple case-insensitive comparison for ASCII letters only. (It
    does not perform full Unicode case folding.)

    Examples:
    - [equal_fold "Go" "go"] returns [true]
    - [equal_fold "AB" "ab"] returns [true]
    - [equal_fold "ß" "ss"] returns [false]

    @param s The first string (UTF-8)
    @param t The second string (UTF-8)
    @return
      [true] if [s] and [t] are equal under simple case folding, [false]
      otherwise *)

val fields : string -> string list
(** [fields s] splits the string [s] around each instance of one or more
    consecutive Unicode whitespace characters, returning a list of substrings of
    [s] or an empty list if [s] contains only whitespace.

    Whitespace is defined by Unicode (see [is_space]).

    Examples:
    - [fields "  foo bar  baz   "] returns [["foo"; "bar"; "baz"]]
    - [fields "   "] returns [[]]
    - [fields "a\tb\nc"] returns [["a"; "b"; "c"]]

    @param s The input string (UTF-8)
    @return List of non-whitespace substrings of [s] *)

val fields_func : string -> (Uchar.t -> bool) -> string list
(** [fields_func s f] splits the string [s] at each run of Unicode code points
    [c] satisfying [f c], returning a list of substrings of [s] or an empty list
    if all code points in [s] satisfy [f] or [s] is empty.

    Examples:
    - [fields_func "  foo1;bar2,baz3..." (fun c -> not (is_letter c || is_number
       c))] returns [["foo1"; "bar2"; "baz3"]]

    @param s The input string (UTF-8)
    @param f The predicate function on Unicode code points
    @return List of non-separator substrings of [s] *)

val index : string -> string -> int
(** [index s substr] returns the index of the first instance of [substr] in [s],
    or [-1] if [substr] is not present.

    The index is a byte offset (not code point index).

    Examples:
    - [index "chicken" "ken"] returns [4]
    - [index "chicken" "dmr"] returns [-1]
    - [index "abc" ""] returns [0]
    - [index "" ""] returns [0]
    - [index "" "a"] returns [-1]

    @param s The input string
    @param substr The substring to search for
    @return The byte index of the first occurrence, or [-1] if not found *)

val repeat : string -> int -> string
(** [repeat s count] returns a new string consisting of [count] copies of [s].

    Raises [Invalid_argument] if [count] is negative.

    Examples:
    - [repeat "na" 2] returns ["nana"]
    - [repeat "🍎" 3] returns ["🍎🍎🍎"]
    - [repeat "" 5] returns [""]
    - [repeat "a" 0] returns [""]
    - [repeat "abc" (-1)] raises [Invalid_argument]

    @param s The string to repeat
    @param count The number of times to repeat [s]
    @return The repeated string *)

val join : string list -> string -> string
(** [join elems sep] concatenates the elements of [elems], inserting [sep]
    between each element.

    Returns the empty string if [elems] is empty.

    Examples:
    - [join ["foo"; "bar"; "baz"] ", "] returns ["foo, bar, baz"]
    - [join [] ", "] returns [""]
    - [join ["a"] ", "] returns ["a"]

    @param elems The list of strings to join
    @param sep The separator string
    @return The joined string *)

val trim : string -> string -> string
(** [trim s cutset] returns [s] with all leading and trailing Unicode code
    points contained in [cutset] removed.

    This function is Unicode-aware and trims by code points, not bytes.

    Examples:
    - [trim "¡¡¡Hello, Camels!!!" "!¡"] returns ["Hello, Camels"]

    @param s The input string (UTF-8)
    @param cutset The set of Unicode code points to trim (UTF-8)
    @return The trimmed string *)

val trim_func : string -> (Uchar.t -> bool) -> string
(** [trim_func s f] returns [s] with all leading and trailing Unicode code
    points [c] satisfying [f c] removed.

    This function is Unicode-aware and trims by code points, not bytes.

    Examples:
    - [trim_func "¡¡¡Hello, Camels!!!" (fun c -> not (is_letter c || is_number
       c))] returns ["Hello, Camels"]

    @param s The input string (UTF-8)
    @param f The predicate function on Unicode code points
    @return The trimmed string *)

val trim_left : string -> string -> string
(** [trim_left s cutset] returns [s] with all leading Unicode code points
    contained in [cutset] removed.

    This function is Unicode-aware and trims by code points, not bytes.

    Examples:
    - [trim_left "¡¡¡Hello, Camels!!!" "!¡"] returns ["Hello, Camels!!!"]

    @param s The input string (UTF-8)
    @param cutset The set of Unicode code points to trim (UTF-8)
    @return The trimmed string *)

val trim_left_func : string -> (Uchar.t -> bool) -> string
(** [trim_left_func s f] returns [s] with all leading Unicode code points [c]
    satisfying [f c] removed.

    This function is Unicode-aware and trims by code points, not bytes.

    Examples:
    - [trim_left_func "¡¡¡Hello, Camels!!!" (fun c -> not (is_letter c ||
       is_number c))] returns ["Hello, Camels!!!"]

    @param s The input string (UTF-8)
    @param f The predicate function on Unicode code points
    @return The trimmed string *)

val trim_right : string -> string -> string
(** [trim_right s cutset] returns [s] with all trailing Unicode code points
    contained in [cutset] removed.

    This function is Unicode-aware and trims by code points, not bytes.

    Examples:
    - [trim_right "¡¡¡Hello, Camels!!!" "!¡"] returns ["¡¡¡Hello, Camels"]

    @param s The input string (UTF-8)
    @param cutset The set of Unicode code points to trim (UTF-8)
    @return The trimmed string *)

val trim_right_func : string -> (Uchar.t -> bool) -> string
(** [trim_right_func s f] returns [s] with all trailing Unicode code points [c]
    satisfying [f c] removed.

    This function is Unicode-aware and trims by code points, not bytes.

    Examples:
    - [trim_right_func "¡¡¡Hello, Camels!!!" (fun c -> not (is_letter c ||
       is_number c))] returns ["¡¡¡Hello, Camels"]

    @param s The input string (UTF-8)
    @param f The predicate function on Unicode code points
    @return The trimmed string *)

val trim_space : string -> string
(** [trim_space s] returns [s] with all leading and trailing Unicode whitespace
    removed.

    This function is Unicode-aware and trims by code points, not bytes.
    Whitespace is defined by Unicode (see [is_space]).

    Examples:
    - [trim_space " \t\n Hello, Camels \n\t\r\n"] returns ["Hello, Camels"]

    @param s The input string (UTF-8)
    @return The trimmed string *)

val trim_suffix : string -> string -> string
(** [trim_suffix s suffix] returns [s] without the provided trailing [suffix]
    string. If [s] does not end with [suffix], [s] is returned unchanged.

    This function is byte-based, not Unicode-aware.

    Examples:
    - [trim_suffix "¡¡¡Hello, Camels!!!" ", Camels!!!"] returns ["¡¡¡Hello"]
    - [trim_suffix "¡¡¡Hello, Camels!!!" ", Marmots!!!"] returns
      ["¡¡¡Hello, Camels!!!"]
    - [trim_suffix "abc" ""] returns ["abc"]

    @param s The input string
    @param suffix The suffix to remove
    @return [s] without the trailing [suffix], or [s] if [suffix] is not present
*)

val to_lower : string -> string
(** [to_lower s] returns [s] with all Unicode letters mapped to their lower
    case.

    This function currently only lowercases ASCII letters (A-Z). Unicode-aware
    lowercasing is not yet implemented.

    Examples:
    - [to_lower "Camel"] returns ["camel"]
    - [to_lower "CAMEL"] returns ["camel"]
    - [to_lower "こんにちは"] returns ["こんにちは"]

    @param s The input string (UTF-8)
    @return The lowercased string *)

val to_title : string -> string
(** [to_title s] returns [s] with all Unicode letters mapped to their Unicode
    title case.

    Currently, only ASCII letters are supported (A-Z, a-z). TODO: Support full
    Unicode title case in the future.

    Examples:
    - [to_title "her royal highness"] returns ["HER ROYAL HIGHNESS"]
    - [to_title "loud noises"] returns ["LOUD NOISES"]
    - [to_title "брат"] returns ["брат"]

    @param s The input string (UTF-8)
    @return The title-cased string *)

val to_upper : string -> string
(** [to_upper s] returns [s] with all Unicode letters mapped to their upper
    case.

    This function currently only uppercases ASCII letters (a-z). TODO: Support
    full Unicode uppercasing in the future.

    Examples:
    - [to_upper "Camel"] returns ["CAMEL"]
    - [to_upper "camel"] returns ["CAMEL"]
    - [to_upper "こんにちは"] returns ["こんにちは"]

    @param s The input string (UTF-8)
    @return The uppercased string *)

val to_camel_case : string -> string
(** Convert words separated by space, underscore, or hyphen to camel case.
    - Words are split on '_', '-', or space.
    - The first word is lowercased (even if originally all uppercase).
    - Subsequent words are capitalized (first letter uppercase, rest lowercase).
    - All-uppercase words are handled (e.g. "GOLANG_IS_GREAT" →
      "golangIsGreat").
    - If there are no separators, the original string is returned (e.g.
      "alreadyCamel" → "alreadyCamel").
    - Leading and trailing underscores are preserved (e.g. "_complex__case_" →
      "_complexCase_").
    - Multiple consecutive separators are treated as a single word boundary.
    - Hyphens and spaces are also treated as word boundaries.

    Examples:
    - to_camel_case "some_words" = "someWords"
    - to_camel_case "_complex__case_" = "_complexCase_"
    - to_camel_case "OCAML_IS_GREAT" = "ocamlIsGreat"
    - to_camel_case "alreadyCamel" = "alreadyCamel"
    - to_camel_case "foo-BarBaz" = "fooBarBaz"
    - to_camel_case "word" = "word"
    - to_camel_case "" = "" *)

val to_kebab_case : string -> string
(** [to_kebab_case s] converts a string to kebab-case.
    - Uppercase ASCII letters are converted to lowercase.
    - Word boundaries are detected at transitions from lowercase to uppercase,
      from letter to digit, and at underscores, spaces, or hyphens.
    - All word boundaries are replaced with a single hyphen '-'.
    - Multiple consecutive separators are treated as a single hyphen.
    - Leading and trailing hyphens are removed.
    - If the input is empty, returns the empty string.

    Examples:
    - to_kebab_case "FirstName" = "first-name"
    - to_kebab_case "HTTPServer" = "http-server"
    - to_kebab_case "NoHTTPS" = "no-https"
    - to_kebab_case "GO_PATH" = "go-path"
    - to_kebab_case "GO PATH" = "go-path"
    - to_kebab_case "GO-PATH" = "go-path"
    - to_kebab_case "http2xx" = "http-2xx"
    - to_kebab_case "HTTP20xOK" = "http-20x-ok"
    - to_kebab_case "Duration2m3s" = "duration-2m-3s"
    - to_kebab_case "Bld4Floor3rd" = "bld4-floor-3rd"
    - to_kebab_case "abc" = "abc"
    - to_kebab_case "A" = "a"
    - to_kebab_case "FooBarBaz" = "foo-bar-baz"
    - to_kebab_case "" = "" *)

val to_pascal_case : string -> string
(** Convert words separated by space, underscore, or hyphen to PascalCase.
    - Words are split on '_', '-', or space.
    - Each word is capitalized (first letter uppercase, rest lowercase).
    - All-uppercase words are handled (e.g. "OCAML_IS_GREAT" → "OcamlIsGreat").
    - If there are no separators, the first letter is uppercased, the rest are
      unchanged.
    - Leading and trailing underscores and separators are removed in the output.
    - Multiple consecutive separators are treated as a single word boundary.
    - Hyphens and spaces are also treated as word boundaries. Examples:
    - to_pascal_case "some_words" = "SomeWords"
    - to_pascal_case "_complex__case_" = "ComplexCase"
    - to_pascal_case "OCAML_IS_GREAT" = "OcamlIsGreat"
    - to_pascal_case "alreadyPascal" = "AlreadyPascal"
    - to_pascal_case "foo-BarBaz" = "FooBarBaz"
    - to_pascal_case "word" = "Word"
    - to_pascal_case "" = "" *)

val to_snake_case : string -> string
(** [to_snake_case s] converts a string to snake_case.
    - Uppercase ASCII letters are converted to lowercase.
    - Word boundaries are detected at transitions from lowercase to uppercase,
      from letter to digit, and at underscores, spaces, or hyphens.
    - All word boundaries are replaced with a single underscore '_'.
    - Multiple consecutive separators are treated as a single underscore.
    - Leading and trailing underscores are removed.
    - If the input is empty, returns the empty string.

    Examples:
    - to_snake_case "FirstName" = "first_name"
    - to_snake_case "HTTPServer" = "http_server"
    - to_snake_case "NoHTTPS" = "no_https"
    - to_snake_case "GO_PATH" = "go_path"
    - to_snake_case "GO PATH" = "go_path"
    - to_snake_case "GO-PATH" = "go_path"
    - to_snake_case "http2xx" = "http_2xx"
    - to_snake_case "HTTP20xOK" = "http_20x_ok"
    - to_snake_case "Duration2m3s" = "duration_2m3s"
    - to_snake_case "Bld4Floor3rd" = "bld4_floor_3rd" *)

val map : (Uchar.t -> Uchar.t) -> string -> string
(** [map f s] returns a new string which is the result of applying [f] to each
    Unicode code point of [s]. The mapping function [f] must return a valid
    [Uchar.t] for every input.

    This function is Unicode-aware: it decodes [s] into code points, applies
    [f], then re-encodes into UTF-8.

    Example: let rot13 u = let c = Uchar.to_int u in if c >= Char.code 'A' && c
    <= Char.code 'Z' then Uchar.of_int (Char.code 'A' + ((c - Char.code 'A' +
    13) mod 26)) else if c >= Char.code 'a' && c <= Char.code 'z' then
    Uchar.of_int (Char.code 'a' + ((c - Char.code 'a' + 13) mod 26)) else u in
    map rot13 "'Twas brillig and the slithy camel..." = "'Gjnf oevyyvt naq gur
    fyvgul pnzry..." *)

val filter_map : (Uchar.t -> Uchar.t option) -> string -> string
(** [filter_map f s] applies [f] to each Unicode code point [u] of [s]. If [f u]
    returns [Some u'], [u'] is kept in the result; if [None], [u] is dropped.

    This function is Unicode-aware: it decodes [s] into code points, applies
    [f], then re-encodes into UTF-8.

    Example: let drop_vowel u = match Uchar.to_int u with | c when List.mem c
    [ Char.code 'a'; Char.code 'e'; Char.code 'i' ; Char.code 'o'; Char.code 'u'
     ] -> None | _ -> Some u in filter_map drop_vowel "hello" = "hll" *)
