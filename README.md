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
    (** Compute Levenshtein edit distance (Unicode-aware). *)
  end

  val center : string -> int -> string -> string
  (** Center a string using a given pad string (Unicode-aware). *)

  val count : string -> string -> int
  (** Count Unicode characters in a string that match a pattern.
      Supports ranges ("a-z"), negation ("^0-9"), and Unicode ranges. *)

  val delete : string -> string -> string
  (** Remove all Unicode characters in a string that match a pattern.
      Supports ranges ("a-z"), negation ("^0-9"), and Unicode ranges. *)

  val len : string -> int
  (** Return the number of Unicode code points (runes) in a UTF-8 string.
      Counts characters, not bytes. Unicode-safe. *)

  val reverse : string -> string
  (** Reverse a UTF-8 encoded string by Unicode code points.
      This is Unicode-aware and works with emoji, Japanese, etc.
      [reverse "hello"] returns ["olleh"]
      [reverse "こんにちは"] returns ["はちにんこ"]
      [reverse "🍎🍏🍊"] returns ["🍊🍏🍎"] *)
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

