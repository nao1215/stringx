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
