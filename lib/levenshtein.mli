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
