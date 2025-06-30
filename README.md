[![Build and Test](https://github.com/nao1215/stringx/actions/workflows/test.yml/badge.svg)](https://github.com/nao1215/stringx/actions/workflows/test.yml)
[![Dependabot Updates](https://github.com/nao1215/stringx/actions/workflows/dependabot/dependabot-updates/badge.svg)](https://github.com/nao1215/stringx/actions/workflows/dependabot/dependabot-updates)


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

  val fields : string -> string list
  (** Split a string by runs of Unicode whitespace. Returns an empty list if only whitespace.
      Example: fields "  foo bar  baz   " = ["foo"; "bar"; "baz"] *)

  val fields_func : string -> (Uchar.t -> bool) -> string list
  (** Split a string at runs of code points where [f] returns true.
      Example: fields_func "foo1;bar2,baz3" (fun c -> not (is_letter c || is_number c)) = ["foo1"; "bar2"; "baz3"] *)

  val has_prefix : string -> string -> bool
  (** Check if [s] starts with [prefix] (byte-based).
      Example: has_prefix "Gopher" "Go" = true *)

  val has_suffix : string -> string -> bool
  (** Check if [s] ends with [suffix] (byte-based).
      Example: has_suffix "Amigo" "go" = true *)

  val index : string -> string -> int
  (** Return the byte offset of the first occurrence of [substr] in [s], or -1 if not found.
      Example: index "chicken" "ken" = 4 *)

  val join : string list -> string -> string
  (** Join a list of strings with a separator.
      Example: join ["foo"; "bar"; "baz"] ", " = "foo, bar, baz" *)

  val len : string -> int
  (** Return the number of Unicode code points (runes) in a UTF-8 string.
      Example: len "🍎🍏🍊" = 3 *)

  val map : (Uchar.t -> int) -> string -> string
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

