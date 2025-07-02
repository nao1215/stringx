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

The latest API documentation is published at:  
👉 [https://nao1215.github.io/stringx/stringx/Stringx/index.html](https://nao1215.github.io/stringx/stringx/Stringx/index.html)

```ocaml
(** Computes the Levenshtein distance between two UTF-8 strings. *)
val distance : s:string -> t:string -> int

(** Centers a string within a specified length, padding as needed. *)
val center : len:int -> pad:string -> string -> string

(** Counts Unicode characters in a string that match a given pattern. *)
val count : pattern:string -> string -> int

(** Deletes Unicode characters from a string that match a given pattern. *)
val delete : pattern:string -> string -> string

(** Returns the number of Unicode code points in a string. *)
val length : string -> int

(** Reverses a UTF-8 encoded string by Unicode code points. *)
val reverse : string -> string

(** Checks if a string contains a specific substring. *)
val contains : substr:string -> string -> bool

(** Checks if a string contains any Unicode code points from a given set. *)
val contains_any : chars:string -> string -> bool

(** Checks if a string starts with a given prefix. *)
val has_prefix : prefix:string -> string -> bool

(** Checks if a string ends with a given suffix. *)
val has_suffix : suffix:string -> string -> bool

(** Counts non-overlapping occurrences of a substring. *)
val count_substring : substr:string -> string -> int

(** Compares two strings for equality, ignoring case (ASCII only). *)
val equal_fold : other:string -> string -> bool

(** Splits a string into a list of words, using whitespace as a delimiter. *)
val fields : string -> string list

(** Splits a string using a custom delimiter function. *)
val fields_func : f:(Uchar.t -> bool) -> string -> string list

(** Finds the first index of a substring. *)
val index : substr:string -> string -> int

(** Repeats a string a specified number of times. *)
val repeat : count:int -> string -> string

(** Joins a list of strings into a single string with a separator. *)
val join : sep:string -> string list -> string

(** Removes leading and trailing characters from a string that are in a given set. *)
val trim : cutset:string -> string -> string

(** Removes leading and trailing characters from a string based on a predicate. *)
val trim_func : f:(Uchar.t -> bool) -> string -> string

(** Removes leading characters from a string that are in a given set. *)
val trim_left : cutset:string -> string -> string

(** Removes leading characters from a string based on a predicate. *)
val trim_left_func : f:(Uchar.t -> bool) -> string -> string

(** Removes trailing characters from a string that are in a given set. *)
val trim_right : cutset:string -> string -> string

(** Removes trailing characters from a string based on a predicate. *)
val trim_right_func : f:(Uchar.t -> bool) -> string -> string

(** Removes leading and trailing whitespace from a string. *)
val trim_space : string -> string

(** Removes a trailing suffix from a string. *)
val trim_suffix : suffix:string -> string -> string

(** Converts a string to lowercase. *)
val to_lower : string -> string

(** Converts a string to title case. *)
val to_title : string -> string

(** Converts a string to uppercase. *)
val to_upper : string -> string

(** Converts a string to camelCase. *)
val to_camel_case : string -> string

(** Converts a string to kebab-case. *)
val to_kebab_case : string -> string

(** Converts a string to PascalCase. *)
val to_pascal_case : string -> string

(** Converts a string to snake_case. *)
val to_snake_case : string -> string

(** Applies a function to each Unicode code point in a string. *)
val map : f:(Uchar.t -> Uchar.t) -> string -> string

(** Maps and filters Unicode code points in a string. *)
val filter_map : f:(Uchar.t -> Uchar.t option) -> string -> string

(** Iterates over the Unicode code points in a string. *)
val iter : f:(Uchar.t -> unit) -> string -> unit

(** Folds over the Unicode code points in a string. *)
val fold : f:('acc -> Uchar.t -> 'acc) -> init:'acc -> string -> 'acc

(** Expands tab characters to spaces. *)
val expand_tabs : tab_size:int -> string -> string

(** Converts the first Unicode code point of a string to lowercase. *)
val first_rune_to_lower : string -> string

(** Converts the first Unicode code point of a string to uppercase. *)
val first_rune_to_upper : string -> string

(** Inserts a string into another at a specified index. *)
val insert : src:string -> index:int -> string -> string

(** Partitions a string by the last occurrence of a separator. *)
val last_partition : sep:string -> string -> string * string * string

(** Left-justifies a string within a specified width. *)
val left_justify : width:int -> pad:string -> string -> string

(** Partitions a string by the first occurrence of a separator. *)
val partition : sep:string -> string -> string * string * string

(** Right-justifies a string within a specified width. *)
val right_justify : width:int -> pad:string -> string -> string

(** Returns the display width of a Unicode code point. *)
val rune_width : Uchar.t -> int

(** Replaces invalid UTF-8 sequences in a string. *)
val scrub : repl:string -> string -> string

(** Randomly shuffles the Unicode code points in a string. *)
val shuffle : string -> string

(** Shuffles a string using a provided random source. *)
val shuffle_source : rand:Random.State.t -> string -> string

(** Extracts a slice of a string by Unicode code point indices. *)
val slice : start:int -> end_:int -> string -> string

(** Removes consecutive repeated characters that match a pattern. *)
val squeeze : pattern:string -> string -> string
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

