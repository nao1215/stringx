val distance : s:string -> t:string -> int
(** [distance ~s ~t] computes the Levenshtein edit distance between two UTF-8
    encoded strings. This function correctly handles multibyte characters,
    including Japanese, Chinese, emoji, and other Unicode symbols.

    The edit distance is the minimum number of single-character edits
    (insertions, deletions, or substitutions) required to change one string into
    the other.

    Example:
    - distance ~s:"kitten" ~t:"sitting" = 3
    - distance ~s:"こんにちは" ~t:"こんばんは" = 2
    - distance ~s:"🍎" ~t:"🍏" = 1

    Malformed UTF-8 sequences are replaced with ['?'] during decoding.

    @param s The first UTF-8 encoded string
    @param t The second UTF-8 encoded string
    @return The edit distance between [s] and [t] *)
