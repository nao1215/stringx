module Levenshtein : sig
  val distance : s:string -> t:string -> int
  (** [distance ~s ~t] computes the Levenshtein (edit) distance between two
      UTF-8 encoded strings.

      The Levenshtein distance is the minimum number of single-character edits
      (insertions, deletions, or substitutions) required to transform one string
      into another.

      This implementation is Unicode-aware and correctly handles multibyte
      characters such as Japanese, Chinese, emoji, and accented letters.

      Examples:
      - [distance ~s:"kitten" ~t:"sitting"] returns [3]
      - [distance ~s:"こんにちは" ~t:"こんばんは"] returns [2]
      - [distance ~s:"🍎" ~t:"🍏"] returns [1]

      Malformed UTF-8 sequences are replaced with ['?'] during decoding.

      @param s The first UTF-8 encoded string
      @param t The second UTF-8 encoded string
      @return The edit distance between [s] and [t] *)
end

val center : len:int -> pad:string -> string -> string
(** [center ~len ~pad s] centers [s] in a string of length [len], padding with
    [pad]. If [s] is longer than [len], it is returned unchanged. Padding is
    inserted symmetrically. [pad] must be non-empty or it is ignored.

    This function is Unicode-aware and counts characters, not bytes. If [pad] is
    multibyte, it is repeated and truncated as needed.

    Examples:
    - [center ~len:10 ~pad:" " "hello"] returns ["  hello   "]
    - [center ~len:7 ~pad:"あ" "abc"] returns ["ああabcああ"]

    @param s The string to center (UTF-8)
    @param len The total length (in Unicode characters) of the result
    @param pad The padding string (UTF-8, non-empty)
    @return The centered string *)

val count : pattern:string -> string -> int
(** [count ~pattern str] counts how many Unicode characters in [str] match
    [pattern].

    The [pattern] supports:
    - character sets: e.g., "aeiou"
    - ranges: e.g., "a-k", "あ-ん"
    - negation with ^: e.g., "^a-k", "^0-9"

    This function is Unicode-aware and handles UTF-8 properly.

    Examples:
    - [count ~pattern:"aeiou" "hello"] returns [2]
    - [count ~pattern:"^a-z" "abc123"] returns [3]
    - [count ~pattern:"あ-ん" "こんにちは"] returns [5]

    @param str The input string (UTF-8)
    @param pattern The character pattern (see above)
    @return The number of matching characters *)

val delete : pattern:string -> string -> string
(** [delete ~pattern str] removes all Unicode characters in [str] that match
    [pattern].

    The [pattern] supports:
    - character sets: e.g., "aeiou"
    - ranges: e.g., "a-k", "あ-ん"
    - negation with ^: e.g., "^a-k", "^0-9"

    This function is Unicode-aware and handles UTF-8 properly.

    Examples:
    - [delete ~pattern:"aeiou" "hello"] returns ["hll"]
    - [delete ~pattern:"こ" "こんにちは"] returns ["んにちは"]
    - [delete ~pattern:"^a-z" "abc123"] returns ["abc"]

    @param str The input string (UTF-8)
    @param pattern The character pattern (see above)
    @return The string with matched characters removed *)

val length : string -> int
(** [length str] returns the number of Unicode code points (runes) in UTF-8
    string [str].

    This function is Unicode-aware and counts characters, not bytes.

    Examples:
    - [length "hello"] returns [5]
    - [length "こんにちは"] returns [5]
    - [length "🍎🍏🍊"] returns [3]

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

val contains : substr:string -> string -> bool
(** [contains ~substr s] reports whether [substr] is within [s].

    Returns [true] if [substr] is the empty string, or if [substr] occurs
    anywhere in [s]. Returns [false] otherwise.

    This function is Unicode-agnostic and operates on bytes, not code points.

    Examples:
    - [contains ~substr:"foo" "seafood"] returns [true]
    - [contains ~substr:"bar" "seafood"] returns [false]
    - [contains ~substr:"" "seafood"] returns [true]
    - [contains ~substr:"" ""] returns [true]

    @param s The input string
    @param substr The substring to search for
    @return [true] if [substr] is found in [s], [false] otherwise *)

val contains_any : chars:string -> string -> bool
(** [contains_any ~chars s] reports whether any Unicode code points in [chars]
    are within [s].

    Returns [false] if [chars] is empty. Unicode-aware and compares by code
    points.

    Examples:
    - [contains_any ~chars:"i" "team"] returns [false]
    - [contains_any ~chars:"ui" "fail"] returns [true]
    - [contains_any ~chars:"ui" "ure"] returns [true]
    - [contains_any ~chars:"ui" "failure"] returns [true]
    - [contains_any ~chars:"" "foo"] returns [false]
    - [contains_any ~chars:"" ""] returns [false]

    @param s The input string (UTF-8)
    @param chars The set of Unicode code points to search for (UTF-8)
    @return
      [true] if any code point in [chars] is found in [s], [false] otherwise *)

val has_prefix : prefix:string -> string -> bool
(** [has_prefix ~prefix s] reports whether the string [s] begins with [prefix].

    Returns [true] if [prefix] is the empty string, or if [s] starts with
    [prefix]. Returns [false] otherwise.

    This function is Unicode-agnostic and operates on bytes, not code points.

    Examples:
    - [has_prefix ~prefix:"Go" "Gopher"] returns [true]
    - [has_prefix ~prefix:"C" "Gopher"] returns [false]
    - [has_prefix ~prefix:"" "Gopher"] returns [true]

    @param s The input string
    @param prefix The prefix to test
    @return [true] if [s] starts with [prefix], [false] otherwise *)

val has_suffix : suffix:string -> string -> bool
(** [has_suffix ~suffix s] reports whether the string [s] ends with [suffix].

    Returns [true] if [suffix] is the empty string, or if [s] ends with
    [suffix]. Returns [false] otherwise.

    This function is Unicode-agnostic and operates on bytes, not code points.

    Examples:
    - [has_suffix ~suffix:"go" "Amigo"] returns [true]
    - [has_suffix ~suffix:"O" "Amigo"] returns [false]
    - [has_suffix ~suffix:"Ami" "Amigo"] returns [false]
    - [has_suffix ~suffix:"" "Amigo"] returns [true]

    @param s The input string
    @param suffix The suffix to test
    @return [true] if [s] ends with [suffix], [false] otherwise *)

val count_substring : substr:string -> string -> int
(** [count_substring ~substr s] counts the number of non-overlapping instances
    of [substr] in [s].

    If [substr] is the empty string, returns 1 + the number of Unicode code
    points in [s].

    This function is Unicode-agnostic and operates on bytes, not code points.

    Examples:
    - [count_substring ~substr:"e" "cheese"] returns [3]
    - [count_substring ~substr:"" "five"] returns [5]
    - [count_substring ~substr:"na" "banana"] returns [2]
    - [count_substring ~substr:"aa" "aaaaa"] returns [2]
    - [count_substring ~substr:"" ""] returns [1]
    - [count_substring ~substr:"a" ""] returns [0]

    @param s The input string
    @param substr The substring to count
    @return The number of non-overlapping instances of [substr] in [s] *)

val equal_fold : other:string -> string -> bool
(** [equal_fold ~other s] reports whether [s] and [other], interpreted as UTF-8
    strings, are equal under simple Unicode case-folding (ASCII only).

    This is a simple case-insensitive comparison for ASCII letters only. (It
    does not perform full Unicode case folding.)

    Examples:
    - [equal_fold ~other:"go" "Go"] returns [true]
    - [equal_fold ~other:"ab" "AB"] returns [true]
    - [equal_fold ~other:"ss" "ß"] returns [false]

    @param s The first string (UTF-8)
    @param other The second string (UTF-8)
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

val fields_func : f:(Uchar.t -> bool) -> string -> string list
(** [fields_func ~f s] splits the string [s] at each run of Unicode code points
    [c] satisfying [f c], returning a list of substrings of [s] or an empty list
    if all code points in [s] satisfy [f] or [s] is empty.

    Examples:
    - [fields_func ~f:(fun c -> not (is_letter c || is_number c)) " 
       foo1;bar2,baz3..."] returns [["foo1"; "bar2"; "baz3"]]

    @param s The input string (UTF-8)
    @param f The predicate function on Unicode code points
    @return List of non-separator substrings of [s] *)

val index : substr:string -> string -> int
(** [index ~substr s] returns the index of the first instance of [substr] in
    [s], or [-1] if [substr] is not present.

    The index is a byte offset (not code point index).

    Examples:
    - [index ~substr:"ken" "chicken"] returns [4]
    - [index ~substr:"dmr" "chicken"] returns [-1]
    - [index ~substr:"" "abc"] returns [0]
    - [index ~substr:"" ""] returns [0]
    - [index ~substr:"a" ""] returns [-1]

    @param s The input string
    @param substr The substring to search for
    @return The byte index of the first occurrence, or [-1] if not found *)

val repeat : count:int -> string -> string
(** [repeat ~count s] returns a new string consisting of [count] copies of [s].

    Raises [Invalid_argument] if [count] is negative.

    Examples:
    - [repeat ~count:2 "na"] returns ["nana"]
    - [repeat ~count:3 "🍎"] returns ["🍎🍎🍎"]
    - [repeat ~count:5 ""] returns [""]
    - [repeat ~count:0 "a"] returns [""]
    - [repeat ~count:(-1) "abc"] raises [Invalid_argument]

    @param s The string to repeat
    @param count The number of times to repeat [s]
    @return The repeated string *)

val join : sep:string -> string list -> string
(** [join ~sep elems] concatenates the elements of [elems], inserting [sep]
    between each element.

    Returns the empty string if [elems] is empty.

    Examples:
    - [join ~sep:", " ["foo"; "bar"; "baz"]] returns ["foo, bar, baz"]
    - [join ~sep:", " []] returns [""]
    - [join ~sep:", " ["a"]] returns ["a"]

    @param elems The list of strings to join
    @param sep The separator string
    @return The joined string *)

val trim : cutset:string -> string -> string
(** [trim ~cutset s] returns [s] with all leading and trailing Unicode code
    points contained in [cutset] removed.

    This function is Unicode-aware and trims by code points, not bytes.

    Examples:
    - [trim ~cutset:"!¡" "¡¡¡Hello, Camels!!!"] returns ["Hello, Camels"]

    @param s The input string (UTF-8)
    @param cutset The set of Unicode code points to trim (UTF-8)
    @return The trimmed string *)

val trim_func : f:(Uchar.t -> bool) -> string -> string
(** [trim_func ~f s] returns [s] with all leading and trailing Unicode code
    points [c] satisfying [f c] removed.

    This function is Unicode-aware and trims by code points, not bytes.

    Examples:
    - [trim_func ~f:(fun c -> not (is_letter c || is_number c)) "¡¡¡Hello,
       Camels!!!"] returns ["Hello, Camels"]

    @param s The input string (UTF-8)
    @param f The predicate function on Unicode code points
    @return The trimmed string *)

val trim_left : cutset:string -> string -> string
(** [trim_left ~cutset s] returns [s] with all leading Unicode code points
    contained in [cutset] removed.

    This function is Unicode-aware and trims by code points, not bytes.

    Examples:
    - [trim_left ~cutset:"!¡" "¡¡¡Hello, Camels!!!"] returns
      ["Hello, Camels!!!"]

    @param s The input string (UTF-8)
    @param cutset The set of Unicode code points to trim (UTF-8)
    @return The trimmed string *)

val trim_left_func : f:(Uchar.t -> bool) -> string -> string
(** [trim_left_func ~f s] returns [s] with all leading Unicode code points [c]
    satisfying [f c] removed.

    This function is Unicode-aware and trims by code points, not bytes.

    Examples:
    - [trim_left_func ~f:(fun c -> not (is_letter c || is_number c)) "¡¡¡Hello,
       Camels!!!"] returns ["Hello, Camels!!!"]

    @param s The input string (UTF-8)
    @param f The predicate function on Unicode code points
    @return The trimmed string *)

val trim_right : cutset:string -> string -> string
(** [trim_right ~cutset s] returns [s] with all trailing Unicode code points
    contained in [cutset] removed.

    This function is Unicode-aware and trims by code points, not bytes.

    Examples:
    - [trim_right ~cutset:"!¡" "¡¡¡Hello, Camels!!!"] returns
      ["¡¡¡Hello, Camels"]

    @param s The input string (UTF-8)
    @param cutset The set of Unicode code points to trim (UTF-8)
    @return The trimmed string *)

val trim_right_func : f:(Uchar.t -> bool) -> string -> string
(** [trim_right_func ~f s] returns [s] with all trailing Unicode code points [c]
    satisfying [f c] removed.

    This function is Unicode-aware and trims by code points, not bytes.

    Examples:
    - [trim_right_func ~f:(fun c -> not (is_letter c || is_number c)) "¡¡¡Hello,
       Camels!!!"] returns ["¡¡¡Hello, Camels"]

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

val trim_suffix : suffix:string -> string -> string
(** [trim_suffix ~suffix s] returns [s] without the provided trailing [suffix]
    string. If [s] does not end with [suffix], [s] is returned unchanged.

    This function is byte-based, not Unicode-aware.

    Examples:
    - [trim_suffix ~suffix:", Camels!!!" "¡¡¡Hello, Camels!!!"] returns
      ["¡¡¡Hello"]
    - [trim_suffix ~suffix:", Marmots!!!" "¡¡¡Hello, Camels!!!"] returns
      ["¡¡¡Hello, Camels!!!"]
    - [trim_suffix ~suffix:"" "abc"] returns ["abc"]

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
    - If there are no separators, the string is lowercased (e.g. "alreadyCamel"
      → "alreadycamel").
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

val map : f:(Uchar.t -> Uchar.t) -> string -> string
(** [map ~f s] returns a new string which is the result of applying [f] to each
    Unicode code point of [s]. The mapping function [f] must return a valid
    [Uchar.t] for every input.

    This function is Unicode-aware: it decodes [s] into code points, applies
    [f], then re-encodes into UTF-8.

    Example: let rot13 u = let c = Uchar.to_int u in if c >= Char.code 'A' && c
    <= Char.code 'Z' then Uchar.of_int (Char.code 'A' + ((c - Char.code 'A' +
    13) mod 26)) else if c >= Char.code 'a' && c <= Char.code 'z' then
    Uchar.of_int (Char.code 'a' + ((c - Char.code 'a' + 13) mod 26)) else u in
    map ~f:rot13 "'Twas brillig and the slithy camel..." = "'Gjnf oevyyvt naq
    gur fyvgul pnzry..." *)

val filter_map : f:(Uchar.t -> Uchar.t option) -> string -> string
(** [filter_map ~f s] applies [f] to each Unicode code point [u] of [s]. If
    [f u] returns [Some u'], [u'] is kept in the result; if [None], [u] is
    dropped.

    This function is Unicode-aware: it decodes [s] into code points, applies
    [f], then re-encodes into UTF-8.

    Example: let drop_vowel u = match Uchar.to_int u with | c when List.mem c
    [ Char.code 'a'; Char.code 'e'; Char.code 'i' ; Char.code 'o'; Char.code 'u'
     ] -> None | _ -> Some u in filter_map ~f:drop_vowel "hello" = "hll" *)

val iter : f:(Uchar.t -> unit) -> string -> unit
(** [iter f s] applies [f] to each Unicode code point of [s], in sequence,
    purely for side-effects. *)

val fold : f:('acc -> Uchar.t -> 'acc) -> init:'acc -> string -> 'acc
(** [fold ~f ~init s] applies [f acc u] to each Unicode code point [u] of [s],
    carrying along an accumulator [acc], and returns the final accumulator. *)

val expand_tabs : tab_size:int -> string -> string
(** [expand_tabs ~tab_size s] expands tab characters ('\t') in [s] to spaces,
    depending on the current column and [tab_size]. The column is reset to zero
    after each newline ('\n'). CJK characters are treated as width 2.

    Raises [Invalid_argument] if [tab_size] <= 0.

    Examples:
    - [expand_tabs ~tab_size:4 "a\tbc\tdef\tghij\tk"] = "a bc def ghij k"
    - [expand_tabs ~tab_size:4 "abcdefg\thij\nk\tl"] = "abcdefg hij\nk l"
    - [expand_tabs ~tab_size:4 "z中\t文\tw"] = "z中 文 w" *)

val first_rune_to_lower : string -> string
(** [first_rune_to_lower s] returns [s] with the first Unicode code point
    converted to lower case if it is an uppercase ASCII letter. Unicode-aware:
    only the first code point is affected, the rest are unchanged.

    Examples:
    - first_rune_to_lower "CamelCase" = "camelCase"
    - first_rune_to_lower "camelCase" = "camelCase"
    - first_rune_to_lower "CAMEL" = "cAMEL"
    - first_rune_to_lower "こんにちは" = "こんにちは" *)

val first_rune_to_upper : string -> string
(** [first_rune_to_upper s] returns [s] with the first Unicode code point
    converted to upper case if it is a lowercase ASCII letter. Unicode-aware:
    only the first code point is affected, the rest are unchanged.

    Examples:
    - first_rune_to_upper "camelCase" = "CamelCase"
    - first_rune_to_upper "CamelCase" = "CamelCase"
    - first_rune_to_upper "camel" = "Camel"
    - first_rune_to_upper "こんにちは" = "こんにちは" *)

val insert : src:string -> index:int -> string -> string
(** [insert ~src ~index dst] inserts [src] into [dst] at the given Unicode code
    point index. Index is counted by code points (runes), not bytes. Raises
    [Invalid_argument] if [index] is out of range (index < 0 or index > length
    of [dst]).

    Examples:
    - [insert ~src:"Super" ~index:5 "CamelCase"] = "CamelSuperCase"
    - [insert ~src:"世界" ~index:2 "こんにちは"] = "こん世界にちは" *)

val last_partition : sep:string -> string -> string * string * string
(** [last_partition ~sep str] splits [str] by the last instance of [sep] into
    three parts: ([head], [match], [tail]). If [sep] is found, [head] is the
    part before the last [sep], [match] is [sep], and [tail] is the part after.
    If [sep] is not found, returns ("", "", [str]). Operates on bytes, not code
    points.

    Examples:
    - [last_partition ~sep:"l" "hello"] = ("hel", "l", "o")
    - [last_partition ~sep:"x" "hello"] = ("", "", "hello") *)

val left_justify : width:int -> pad:string -> string -> string
(** [left_justify ~width ~pad s] returns [s] left-justified in a string of
    [width] Unicode code points, padding with [pad] on the right if needed. If
    [s] is longer than [width], it is returned unchanged. If [pad] is empty, [s]
    is returned unchanged. Padding is truncated as needed. Unicode-aware: counts
    code points, not bytes.

    Examples:
    - [left_justify ~width:4 ~pad:" " "hello"] = "hello"
    - [left_justify ~width:10 ~pad:" " "hello"] = "hello "
    - [left_justify ~width:10 ~pad:"123" "hello"] = "hello12312" *)

val partition : sep:string -> string -> string * string * string
(** [partition ~sep str] splits [str] by the first instance of [sep] into three
    parts: ([head], [match], [tail]). If [sep] is found, [head] is the part
    before the first [sep], [match] is [sep], and [tail] is the part after. If
    [sep] is not found, returns ([str], "", ""). Operates on bytes, not code
    points.

    Examples:
    - [partition ~sep:"l" "hello"] = ("he", "l", "lo")
    - [partition ~sep:"x" "hello"] = ("hello", "", "") *)

val right_justify : width:int -> pad:string -> string -> string
(** [right_justify ~width ~pad s] returns [s] right-justified in a string of
    [width] Unicode code points, padding with [pad] on the left if needed. If
    [s] is longer than [width], it is returned unchanged. If [pad] is empty, [s]
    is returned unchanged. Padding is truncated as needed. Unicode-aware: counts
    code points, not bytes.

    Examples:
    - [right_justify ~width:4 ~pad:" " "hello"] = "hello"
    - [right_justify ~width:10 ~pad:" " "hello"] = " hello"
    - [right_justify ~width:10 ~pad:"123" "hello"] = "12312hello" *)

val rune_width : Uchar.t -> int
(** [rune_width u] returns the character width of Unicode code point [u] in a
    monotype font. Multi-byte (East Asian wide) characters are usually twice the
    width of single byte characters.

    The algorithm is based on PHP's mb_strwidth. See:
    http://php.net/manual/en/function.mb-strwidth.php

    @param u The Unicode code point
    @return The width (1 or 2) *)

val scrub : repl:string -> string -> string
(** [scrub ~repl str] replaces invalid UTF-8 byte sequences in [str] with
    [repl]. Adjacent invalid bytes are replaced only once. Unicode-aware.

    Examples:
    - [scrub ~repl:"?" "a\xffb"] returns ["a?b"]
    - [scrub ~repl:"?" "a\xff\xffb"] returns ["a?b"]
    - [scrub ~repl:"?" "a\xffb\xff"] returns ["a?b?"]
    - [scrub ~repl:"?" "abc"] returns ["abc"]

    @param str The input string (possibly invalid UTF-8)
    @param repl The replacement string for invalid bytes
    @return The scrubbed string *)

val shuffle : string -> string
(** [shuffle str] randomizes the order of Unicode code points in [str] and
    returns the result. Uses OCaml's Random module as the random source. This is
    equivalent to PHP's str_shuffle. Unicode-aware: shuffles by code points, not
    bytes.

    Examples:
    - [shuffle "Camel"] might return ["eCaml"], ["lCema"], etc.
    - [shuffle "こんにちは"] might return ["にちこんは"], etc.

    @param str The input string (UTF-8)
    @return The shuffled string *)

val shuffle_source : rand:Random.State.t -> string -> string
(** [shuffle_source str rand_state] randomizes the order of Unicode code points
    in [str] using the given [Random.State.t] as the random source. This is
    equivalent to PHP's str_shuffle. Unicode-aware: shuffles by code points, not
    bytes.

    Examples:
    - [shuffle_source ~rand:(Random.State.make [|42|]) "Camel"] might return
      ["eCaml"], ["lCema"], etc.
    - [shuffle_source ~rand:(Random.State.make [|42|]) "こんにちは"] might return
      ["にちこんは"], etc.

    @param str The input string (UTF-8)
    @param rand The random state to use for shuffling
    @return The shuffled string *)

val slice : start:int -> end_:int -> string -> string
(** [slice ~start ~end_ str] returns the substring of [str] from code point
    index [start] (inclusive) to [end_] (exclusive). Indexing is by Unicode code
    points, not bytes.

    - [start] must satisfy 0 <= start <= rune length.
    - [end_] can be positive, zero, or negative.
    - If [end_] >= 0, then start <= end_ <= rune length.
    - If [end_] < 0, it means slice to the end of string.

    Raises [Invalid_argument] if indices are out of range.

    This is equivalent to PHP's mb_substr.

    Examples:
    - [slice ~start:0 ~end_:5 "CamelCase"] returns ["Camel"]
    - [slice ~start:5 ~end_:(-1) "CamelCase"] returns ["Case"]
    - [slice ~start:2 ~end_:4 "こんにちは"] returns ["にち"]
    - [slice ~start:2 ~end_:(-1) "こんにちは"] returns ["にちは"]

    @param str The input string (UTF-8)
    @param start The start code point index (inclusive)
    @param end_
      The end code point index (exclusive), or negative for end of string
    @return The sliced substring *)

val squeeze : pattern:string -> string -> string
(** [squeeze ~pattern str] deletes adjacent repeated Unicode code points in
    [str]. If [pattern] is not empty, only code points matching [pattern] are
    squeezed. Unicode-aware: operates on code points, not bytes.

    This is equivalent to Ruby's String#squeeze.

    Examples:
    - [squeeze ~pattern:"" "hello"] returns ["helo"]
    - [squeeze ~pattern:"m-z" "hello"] returns ["hello"]
    - [squeeze ~pattern:" " "hello   world"] returns ["hello world"]

    @param str The input string (UTF-8)
    @param pattern The pattern of code points to squeeze (UTF-8, can be empty)
    @return The squeezed string *)
