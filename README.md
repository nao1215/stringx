# Unicode-aware string utilities in OCaml

**stringx** is a OCaml library that brings robust Unicode (UTF-8) support to your string processing tasks.

> [!NOTE]
> This project is inspired by [huandu/xstrings](https://github.com/huandu/xstrings).  
>
> The goal is to bring string manipulation features found in other programming languages to OCaml. As I am new to OCaml, I welcome your feedback and suggestions 😊

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

  val repeat : string -> int -> string
  (** Return a new string consisting of [count] copies of [s].
      Raises [Invalid_argument] if [count] is negative.
      Example: repeat "na" 2 = "nana" *)

  val reverse : string -> string
  (** Reverse a UTF-8 encoded string by Unicode code points.
      Example: reverse "こんにちは" = "はちにんこ" *)

  val trim : string -> string -> string
  (** Trim all leading and trailing Unicode code points in [cutset] from [s].
      Unicode-aware.
      Example: trim "¡¡¡Hello, Gophers!!!" "!¡" = "Hello, Gophers" *)

  val trim_func : string -> (Uchar.t -> bool) -> string
  (** Trim all leading and trailing Unicode code points in [s] that satisfy [f].
      Unicode-aware.
      Example: trim_func "¡¡¡Hello, Gophers!!!" (fun c -> not (is_letter c || is_number c)) = "Hello, Gophers" *)

  val trim_left : string -> string -> string
  (** Trim all leading Unicode code points in [cutset] from [s].
      Unicode-aware.
      Example: trim_left "¡¡¡Hello, Gophers!!!" "!¡" = "Hello, Gophers!!!" *)

  val trim_left_func : string -> (Uchar.t -> bool) -> string
  (** Trim all leading Unicode code points in [s] that satisfy [f].
      Unicode-aware.
      Example: trim_left_func "¡¡¡Hello, Gophers!!!" (fun c -> not (is_letter c || is_number c)) = "Hello, Gophers!!!" *)
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

