[![Build and Test](https://github.com/nao1215/stringx/actions/workflows/test.yml/badge.svg)](https://github.com/nao1215/stringx/actions/workflows/test.yml)
[![Dependabot Updates](https://github.com/nao1215/stringx/actions/workflows/dependabot/dependabot-updates/badge.svg)](https://github.com/nao1215/stringx/actions/workflows/dependabot/dependabot-updates)
![OPAM](https://img.shields.io/opam/v/stringx)
![OPAM Downloads](https://img.shields.io/opam/dt/stringx)
![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)
![GitHub stars](https://img.shields.io/github/stars/nao1215/stringx?style=social)
[![Documentation](https://img.shields.io/badge/docs-online-blue)](https://nao1215.github.io/stringx/)


# Unicode-aware string utilities in OCaml

**stringx** is a OCaml library that brings robust Unicode (UTF-8) support to your string processing tasks.

> [!NOTE]
> This project is inspired by [huandu/xstrings](https://github.com/huandu/xstrings).  
>
> The goal is to bring string manipulation features found in other programming languages to OCaml. **This library is under active development.**  
> Function proposals and contributions for new string utilities are very welcome! As I am new to OCaml, I welcome your feedback and suggestions 😊

With stringx, you get:
- Accurate edit distance (Levenshtein) calculations for any language or emoji
- Smart centering of strings, even with multibyte characters
- Flexible character counting and deletion using intuitive patterns, ranges, and negation


---

## ✨ Features

- **Unicode-first:** All functions are fully UTF-8 aware—works perfectly with emoji, Japanese, Chinese, and more!
- **Flexible pattern matching:** Use character sets, ranges (e.g. `"a-z"` or `"あ-ん"`), and negation (`"^0-9"`) for powerful string operations.
- **No C bindings:** Pure OCaml, easy to install and portable.
- **Battle-tested:** Includes comprehensive tests for edge cases and malformed UTF-8.

---

## 🔧 API Overview

The latest API documentation is published at:  
👉 [https://nao1215.github.io/stringx/stringx/Stringx/index.html](https://nao1215.github.io/stringx/stringx/Stringx/index.html)

```ocaml
module Stringx : sig
  module Levenshtein : sig
    val distance : string -> string -> int
    (** Compute Levenshtein edit distance (Unicode-aware).
        Example: distance "kitten" "sitting" = 3 *)
  end

  val center : string -> int -> string -> string
  (** Center a string using a given pad string (Unicode-aware).
      Example: center "hello" 10 " " = "  hello   " *)

  val contains : string -> string -> bool
  (** Check if [substr] is present in [s] (byte-based).
      Example: contains "seafood" "foo" = true *)

  val contains_any : string -> string -> bool
  (** Check if any Unicode code point in [chars] is present in [s].
      Example: contains_any "fail" "ui" = true *)

  val count : string -> string -> int
  (** Count Unicode characters in a string that match a pattern.
      Supports ranges ("a-z"), negation ("^0-9"), and Unicode ranges.
      Example: count "hello" "aeiou" = 2 *)

  val count_substring : string -> string -> int
  (** Count non-overlapping occurrences of [substr] in [s] (byte-based).
      Example: count_substring "cheese" "e" = 3 *)

  val delete : string -> string -> string
  (** Remove all Unicode characters in a string that match a pattern.
      Supports ranges ("a-z"), negation ("^0-9"), and Unicode ranges.
      Example: delete "hello" "aeiou" = "hll" *)

  val equal_fold : string -> string -> bool
  (** Compare two strings for equality, ignoring ASCII case.
      Example: equal_fold "Go" "go" = true *)

  val expand_tabs : string -> int -> string
  (** Expand tab characters ('\\t') in the string to spaces, depending on the current column and tab size.
      The column is reset to zero after each newline ('\\n'). CJK characters are treated as width 2.
      Raises [Invalid_argument] if [tab_size] <= 0.
      Example: expand_tabs "a\\tbc\\tdef\\tghij\\tk" 4 = "a   bc  def ghij    k" *)

  val first_rune_to_lower : string -> string
  (** Convert the first Unicode code point to lower case if it is an uppercase ASCII letter.
      Unicode-aware: only the first code point is affected, the rest are unchanged.
      Examples:
      - first_rune_to_lower "CamelCase" = "camelCase"
      - first_rune_to_lower "CAMEL" = "cAMEL"
      - first_rune_to_lower "こんにちは" = "こんにちは"
  *)

  val first_rune_to_upper : string -> string
  (** Convert the first Unicode code point to upper case if it is a lowercase ASCII letter.
      Unicode-aware: only the first code point is affected, the rest are unchanged.
      Examples:
      - first_rune_to_upper "camelCase" = "CamelCase"
      - first_rune_to_upper "CamelCase" = "CamelCase"
      - first_rune_to_upper "camel" = "Camel"
      - first_rune_to_upper "こんにちは" = "こんにちは"
  *)

  val fields : string -> string list
  (** Split a string by runs of Unicode whitespace. Returns an empty list if only whitespace.
      Example: fields "  foo bar  baz   " = ["foo"; "bar"; "baz"] *)

  val fields_func : string -> (Uchar.t -> bool) -> string list
  (** Split a string at runs of code points where [f] returns true.
      Example: fields_func "foo1;bar2,baz3" (fun c -> not (is_letter c || is_number c)) = ["foo1"; "bar2"; "baz3"] *)

  val filter_map : (Uchar.t -> Uchar.t option) -> string -> string
  (** Return a new string by applying the given function to each Unicode code point in the input string.
      If the function returns [Some u'], [u'] is included in the result; if [None], the code point is dropped.
      Unicode-aware: decodes the string into code points, applies the function, then re-encodes into UTF-8.
      Example: let drop_vowel u = match Uchar.to_int u with
        | c when List.mem c [Char.code 'a'; Char.code 'e'; Char.code 'i'; Char.code 'o'; Char.code 'u'] -> None
        | _ -> Some u
      in filter_map drop_vowel "hello" = "hll" *)

  val fold : ('acc -> Uchar.t -> 'acc) -> 'acc -> string -> 'acc
  (** Apply the given function to each Unicode code point in the string, carrying along an accumulator.
      Returns the final accumulator value.
      Unicode-aware: decodes the string into code points and applies the function to each.
      Example: fold (fun acc _ -> acc + 1) 0 "hello" = 5 *)

  val has_prefix : string -> string -> bool
  (** Check if [s] starts with [prefix] (byte-based).
      Example: has_prefix "Gopher" "Go" = true *)

  val has_suffix : string -> string -> bool
  (** Check if [s] ends with [suffix] (byte-based).
      Example: has_suffix "Amigo" "go" = true *)

  val index : string -> string -> int
  (** Return the byte offset of the first occurrence of [substr] in [s], or -1 if not found.
      Example: index "chicken" "ken" = 4 *)

  val insert : string -> string -> int -> string
  (** Insert [src] into [dst] at the given Unicode code point index.
      Index is counted by code points (runes), not bytes.
      Raises [Invalid_argument] if [index] is out of range (index < 0 or index > length of [dst]).
      Examples:
      - insert "CamelCase" "Super" 5 = "CamelSuperCase"
      - insert "こんにちは" "世界" 2 = "こん世界にちは"
  *)

  val iter : (Uchar.t -> unit) -> string -> unit
  (** Apply the given function to each Unicode code point in the string, in sequence, for side effects only.
      Unicode-aware: decodes the string into code points and applies the function to each.
      Example: iter (fun u -> print_endline (Uchar.to_string u)) "abc😊" *)

  val join : string list -> string -> string
  (** Join a list of strings with a separator.
      Example: join ["foo"; "bar"; "baz"] ", " = "foo, bar, baz" *)

  val last_partition : string -> string -> string * string * string
  (** Split [str] by the last instance of [sep] into three parts: ([head], [match], [tail]).
      If [sep] is found, [head] is the part before the last [sep], [match] is [sep], and [tail] is the part after.
      If [sep] is not found, returns ("", "", [str]). Operates on bytes, not code points.
      Examples:
      - last_partition "hello" "l" = ("hel", "l", "o")
      - last_partition "hello" "x" = ("", "", "hello")
  *)

  val left_justify : string -> int -> string -> string
  (** Left-justify [s] in a string of [width] Unicode code points, padding with [pad] on the right if needed.
      If [s] is longer than [width], it is returned unchanged.
      If [pad] is empty, [s] is returned unchanged. Padding is truncated as needed.
      Unicode-aware: counts code points, not bytes.
      Examples:
      - left_justify "hello" 4 " " = "hello"
      - left_justify "hello" 10 " " = "hello     "
      - left_justify "hello" 10 "123" = "hello12312"
  *)

  val right_justify : string -> int -> string -> string
  (** Right-justify [s] in a string of [width] Unicode code points, padding with [pad] on the left if needed.
      If [s] is longer than [width], it is returned unchanged.
      If [pad] is empty, [s] is returned unchanged. Padding is truncated as needed.
      Unicode-aware: counts code points, not bytes.
      Examples:
      - right_justify "hello" 4 " " = "hello"
      - right_justify "hello" 10 " " = "     hello"
      - right_justify "hello" 10 "123" = "12312hello"
  *)

  val partition : string -> string -> string * string * string
  (** Split [str] by the first instance of [sep] into three parts: ([head], [match], [tail]).
      If [sep] is found, [head] is the part before the first [sep], [match] is [sep], and [tail] is the part after.
      If [sep] is not found, returns ([str], "", ""). Operates on bytes, not code points.
      Examples:
      - partition "hello" "l" = ("he", "l", "lo")
      - partition "hello" "x" = ("hello", "", "")
  *)

  val len : string -> int
  (** Return the number of Unicode code points (runes) in a UTF-8 string.
      Example: len "🍎🍏🍊" = 3 *)

  val map : (Uchar.t -> Uchar.t) -> string -> string
  (** Return a copy of the string with all Unicode code points mapped by the given function.
      The mapping function must return a valid Unicode code point (Uchar.t) for every input; no code points are dropped.
      Unicode-aware: decodes the string into code points, applies the function, then re-encodes into UTF-8.
      Example: map rot13 "'Twas brillig and the slithy camel..." = "'Gjnf oevyyvt naq gur fyvgul pnzry..." *)

  val repeat : string -> int -> string
  (** Return a new string consisting of [count] copies of [s].
      Raises [Invalid_argument] if [count] is negative.
      Example: repeat "na" 2 = "nana" *)

  val reverse : string -> string
  (** Reverse a UTF-8 encoded string by Unicode code points.
      Example: reverse "こんにちは" = "はちにんこ" *)

  val to_camel_case : string -> string
  (** Convert words separated by space, underscore, or hyphen to camel case.
      Leading, trailing, and duplicate separators are preserved.
      Example: to_camel_case "some_words" = "someWords"
      Example: to_camel_case "OCAML_IS_GREAT" = "ocamlIsGreat" *)

  val to_kebab_case : string -> string
  (** Convert a string to kebab-case.
      - Uppercase ASCII letters are converted to lowercase.
      - Word boundaries are detected at transitions from lowercase to uppercase, from letter to digit, and at underscores, spaces, or hyphens.
      - All word boundaries are replaced with a single hyphen '-'.
      - Multiple consecutive separators are treated as a single hyphen.
      - Leading and trailing hyphens are removed.
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
      - to_kebab_case "" = ""
  *)

  val to_pascal_case : string -> string
  (** Convert words separated by space, underscore, or hyphen to PascalCase.
      - Words are split on '_', '-', or space.
      - Each word is capitalized (first letter uppercase, rest lowercase).
      - All-uppercase words are handled (e.g. "OCAML_IS_GREAT" → "OcamlIsGreat").
      - If there are no separators, the first letter is uppercased, the rest are unchanged.
      - Leading and trailing underscores are preserved (e.g. "_complex__case_" → "_ComplexCase_").
      - Multiple consecutive separators are treated as a single word boundary.
      - Hyphens and spaces are also treated as word boundaries.
      Examples:
      - to_pascal_case "some_words" = "SomeWords"
      - to_pascal_case "_complex__case_" = "_ComplexCase_"
      - to_pascal_case "OCAML_IS_GREAT" = "OcamlIsGreat"
      - to_pascal_case "alreadyPascal" = "AlreadyPascal"
      - to_pascal_case "foo-BarBaz" = "FooBarBaz"
      - to_pascal_case "word" = "Word"
      - to_pascal_case "" = ""
  *)

  val to_snake_case : string -> string
  (** Convert a string to snake_case.
      - Uppercase ASCII letters are converted to lowercase.
      - Word boundaries are detected at transitions from lowercase to uppercase, from letter to digit, and at underscores, spaces, or hyphens.
      - All word boundaries are replaced with a single underscore '_'.
      - Multiple consecutive separators are treated as a single underscore.
      - Leading and trailing underscores are removed.
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
      - to_snake_case "Bld4Floor3rd" = "bld4_floor_3rd"
      - to_snake_case "abc" = "abc"
      - to_snake_case "A" = "a"
      - to_snake_case "FooBarBaz" = "foo_bar_baz"
      - to_snake_case "" = ""
  *)

  val to_lower : string -> string
  (** Convert all Unicode letters in [s] to lower case (ASCII only).
      Example: to_lower "Camel" = "camel" *)

  val to_title : string -> string
  (** Convert all Unicode letters in [s] to Unicode title case (ASCII only).
      TODO: Support full Unicode title case in the future.
      Example: to_title "her royal highness" = "HER ROYAL HIGHNESS" *)

  val to_upper : string -> string
  (** Convert all Unicode letters in [s] to upper case (ASCII only).
      Example: to_upper "Camel" = "CAMEL" *)

  val trim : string -> string -> string
  (** Trim all leading and trailing Unicode code points in [cutset] from [s].
      Unicode-aware.
      Example: trim "¡¡¡Hello, Camels!!!" "!¡" = "Hello, Camels" *)

  val trim_func : string -> (Uchar.t -> bool) -> string
  (** Trim all leading and trailing Unicode code points in [s] that satisfy [f].
      Unicode-aware.
      Example: trim_func "¡¡¡Hello, Camels!!!" (fun c -> not (is_letter c || is_number c)) = "Hello, Camels" *)

  val trim_left : string -> string -> string
  (** Trim all leading Unicode code points in [cutset] from [s].
      Unicode-aware.
      Example: trim_left "¡¡¡Hello, Camels!!!" "!¡" = "Hello, Camels!!!" *)

  val trim_left_func : string -> (Uchar.t -> bool) -> string
  (** Trim all leading Unicode code points in [s] that satisfy [f].
      Unicode-aware.
      Example: trim_left_func "¡¡¡Hello, Camels!!!" (fun c -> not (is_letter c || is_number c)) = "Hello, Camels!!!" *)

  val trim_right : string -> string -> string
  (** Trim all trailing Unicode code points in [cutset] from [s].
      Unicode-aware.
      Example: trim_right "¡¡¡Hello, Camels!!!" "!¡" = "¡¡¡Hello, Camels" *)

  val trim_right_func : string -> (Uchar.t -> bool) -> string
  (** Trim all trailing Unicode code points in [s] that satisfy [f].
      Unicode-aware.
      Example: trim_right_func "¡¡¡Hello, Camels!!!" (fun c -> not (is_letter c || is_number c)) = "¡¡¡Hello, Camels" *)

  val trim_space : string -> string
  (** Trim all leading and trailing Unicode whitespace from [s].
      Unicode-aware.
      Example: trim_space " \t\n Hello, Camels \n\t\r\n" = "Hello, Camels" *)

  val trim_suffix : string -> string -> string
  (** Remove the provided trailing [suffix] from [s] if present (byte-based).
      Example: trim_suffix "¡¡¡Hello, Camels!!!" ", Camels!!!" = "¡¡¡Hello" *)

  val rune_width : Uchar.t -> int
  (** [rune_width u] returns the character width of Unicode code point [u] in a monotype font.
      Multi-byte (East Asian wide) characters are usually twice the width of single byte characters.

      The algorithm is based on PHP's mb_strwidth.
      See: http://php.net/manual/en/function.mb-strwidth.php

      Example:
      - rune_width (Uchar.of_int 0x3042) = 2  (* Hiragana 'あ' *)
      - rune_width (Uchar.of_int (Char.code 'a')) = 1
  *)

  val scrub : string -> string -> string
  (** [scrub str repl] replaces invalid UTF-8 byte sequences in [str] with [repl].
      Adjacent invalid bytes are replaced only once.
      Unicode-aware.
      Examples:
      - scrub "a\xffb" "?" = "a?b"
      - scrub "a\xff\xffb" "?" = "a?b"
      - scrub "a\xffb\xff" "?" = "a?b?"
      - scrub "abc" "?" = "abc"
  *)

  val shuffle : string -> string
  (** Randomize the order of Unicode code points in a string.
      Uses OCaml's Random module as the random source.
      Unicode-aware: shuffles by code points, not bytes.
      Example: shuffle "Camel" might return "eCaml", "lCema", etc.
      Example: shuffle "こんにちは" might return "にちこんは", etc.
  *)

  val shuffle_source : string -> Random.State.t -> string
  (** Randomize the order of Unicode code points in a string using the given random state.
      Uses [Random.State.t] as the random source.
      Unicode-aware: shuffles by code points, not bytes.
      Example: shuffle_source "Camel" (Random.State.make [|42|]) might return "eCaml", "lCema", etc.
      Example: shuffle_source "こんにちは" (Random.State.make [|42|]) might return "にちこんは", etc.
  *)

  val slice : string -> int -> int -> string
  (** Slice a string by Unicode code points (runes).
      Returns the substring from [start] (inclusive) to [end_] (exclusive).
      - [start] must satisfy 0 <= start <= rune length.
      - [end_] can be positive, zero, or negative.
        - If [end_] >= 0, then start <= end_ <= rune length.
        - If [end_] < 0, it means slice to the end of string.
      Raises [Invalid_argument] if indices are out of range.
      This is equivalent to PHP's mb_substr.
      Examples:
      - slice "CamelCase" 0 5 = "Camel"
      - slice "CamelCase" 5 (-1) = "Case"
      - slice "こんにちは" 2 4 = "にち"
      - slice "こんにちは" 2 (-1) = "にちは"
  *)

  val squeeze : string -> string -> string
  (** Delete adjacent repeated Unicode code points in a string.
      If [pattern] is not empty, only code points matching [pattern] are squeezed.
      Unicode-aware: operates on code points, not bytes.
      This is equivalent to Ruby's String#squeeze.
      Examples:
      - squeeze "hello" "" = "helo"
      - squeeze "hello" "m-z" = "hello"
      - squeeze "hello   world" " " = "hello world"
  *)
end
```

---

## 📦 Installation

Using [opam](https://opam.ocaml.org/):

```sh
opam pin add stringx https://github.com/nao1215/stringx.git
```

Once released:

```sh
opam install stringx
```

---

## 🛠 Build from Source

```sh
git clone https://github.com/nao1215/stringx.git
cd stringx

opam install . --deps-only -y
dune build
```

Run tests:

```sh
dune runtest
```

Generate documentation:

```sh
opam install odoc
dune build @doc
xdg-open _build/default/_doc/_html/index.html
```

---

## 🧑‍💻 Contributing

1. Format your code with `ocamlformat`.
2. Make sure `dune runtest` passes.
3. Add tests and documentation for new features.

Pull requests and issues welcome:
👉 [https://github.com/nao1215/stringx/issues](https://github.com/nao1215/stringx/issues)

---

## 📝 License

This project is licensed under the [MIT License](./LICENSE).

