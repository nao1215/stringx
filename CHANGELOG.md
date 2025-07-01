# Changelog

## [Unreleased]

- add: `map`  
  Returns a copy of the string with all Unicode code points mapped by the given function.  The mapping function must return a valid Unicode code point (`Uchar.t`) for every input; no code points are dropped. Unicode-aware: decodes the string into code points, applies the function, then re-encodes into UTF-8.

- add: `filter_map`  
  Returns a new string by applying the given function to each Unicode code point in the input string.  
  If the function returns [Some u'], [u'] is included in the result; if [None], the code point is dropped.  
  Unicode-aware: decodes the string into code points, applies the function, then re-encodes into UTF-8.

- add: `iter`  
  Applies the given function to each Unicode code point in the string, in sequence, for side effects only.  
  Unicode-aware: decodes the string into code points and applies the function to each.

- add: `fold`  
  Applies the given function to each Unicode code point in the string, carrying along an accumulator, and returns the final accumulator value.  
  Unicode-aware: decodes the string into code points and applies the function to each.

- add: `expand_tabs`  
  Expands tab characters ('\t') in the string to spaces, depending on the current column and tab size.  
  The column is reset to zero after each newline ('\n'). CJK characters are treated as width 2.  
  Raises [Invalid_argument] if [tab_size] <= 0.

- add: `first_rune_to_lower`  
  Converts the first Unicode code point to lower case if it is an uppercase ASCII letter.  
  Unicode-aware: only the first code point is affected, the rest are unchanged.

- add: `first_rune_to_upper`  
  Converts the first Unicode code point to upper case if it is a lowercase ASCII letter.  
  Unicode-aware: only the first code point is affected, the rest are unchanged.

- add: `insert`  
  Inserts [src] into [dst] at the given Unicode code point index.  
  Index is counted by code points (runes), not bytes.  
  Raises [Invalid_argument] if [index] is out of range (index < 0 or index > length of [dst]).

- add: `last_partition`  
  Splits a string by the last instance of a separator into three parts: ([head], [match], [tail]).  
  If the separator is found, returns the part before, the separator itself, and the part after.  
  If not found, returns ("", "", [str]). Operates on bytes, not code points.

- add: `left_justify`  
  Left-justifies a string in a field of the given width, padding with the given pad string on the right if needed.  
  If the string is longer than the width or pad is empty, the original string is returned.  
  Unicode-aware: counts code points, not bytes.

- add: `partition`  
  Splits a string by the first instance of a separator into three parts: ([head], [match], [tail]).  
  If the separator is found, returns the part before, the separator itself, and the part after.  
  If not found, returns ([str], "", ""). Operates on bytes, not code points.

- add: `right_justify`  
  Right-justifies a string in a field of the given width, padding with the given pad string on the left if needed.  
  If the string is longer than the width or pad is empty, the original string is returned.  
  Unicode-aware: counts code points, not bytes.

- add: `rune_width`
  Returns the character width of a Unicode code point in a monotype font. Multi-byte (East Asian wide) characters are usually twice the width of single byte characters. Algorithm based on PHP's mb_strwidth.

- add: `scrub`  
  Replaces invalid UTF-8 byte sequences in a string with a replacement string. Adjacent invalid bytes are replaced only once. Unicode-aware.

- add: `shuffle`  
  Randomizes the order of Unicode code points in a string. Uses OCaml's Random module as the random source. Unicode-aware: shuffles by code points, not bytes. Equivalent to PHP's str_shuffle.

- add: `shuffle_source`  
  Randomizes the order of Unicode code points in a string using the given random state ([Random.State.t]). Unicode-aware: shuffles by code points, not bytes. Equivalent to PHP's str_shuffle with custom random source.

- add: `slice`  
  Slices a string by Unicode code points (runes). Returns the substring from [start] (inclusive) to [end_] (exclusive). Negative [end_] means slice to the end of string. Equivalent to PHP's mb_substr. Raises [Invalid_argument] if indices are out of range.

## [v0.2.0] - 2025-06-29

- add: `trim`  
  Removes all leading and trailing Unicode code points in the given cutset from the string. Unicode-aware.

- add: `trim_func`  
  Removes all leading and trailing Unicode code points from the string that satisfy the given predicate function. Unicode-aware.

- add: `trim_left`  
  Removes all leading Unicode code points in the given cutset from the string. Unicode-aware.

- add: `trim_left_func`  
  Removes all leading Unicode code points from the string that satisfy the given predicate function. Unicode-aware.

- add: `trim_right`  
  Removes all trailing Unicode code points in the given cutset from the string. Unicode-aware.

- add: `trim_right_func`  
  Removes all trailing Unicode code points from the string that satisfy the given predicate function. Unicode-aware.

- add: `trim_space`  
  Removes all leading and trailing Unicode whitespace from the string. Unicode-aware.

- add: `trim_suffix`  
  Removes the provided trailing suffix, if present. Operates on bytes, not code points.

- add: `to_lower`  
  Converts all ASCII letters in the string to lowercase. (Full Unicode lowercasing not yet supported.)

- add: `to_title`  
  Converts all ASCII letters in the string to uppercase (title case). (Full Unicode title case not yet supported.)

- add: `to_upper`  
  Converts all ASCII letters in the string to uppercase. (Full Unicode uppercasing not yet supported.)

- add: `to_camel_case`  
  Converts words separated by space, underscore, or hyphen to camelCase. The first word is lowercased, subsequent words are capitalized. Handles all-uppercase words and preserves leading/trailing separators.

- add: `to_kebab_case`  
  Converts a string to kebab-case. Uppercase ASCII letters are converted to lowercase. Word boundaries are detected at transitions and replaced with a single hyphen. Multiple separators are normalized, and leading/trailing hyphens are removed.

- add: `to_pascal_case`  
  Converts words separated by space, underscore, or hyphen to PascalCase. Each word is capitalized. Handles all-uppercase words and removes leading/trailing separators.

- add: `to_snake_case`  
  Converts a string to snake_case. Uppercase ASCII letters are converted to lowercase. Word boundaries are detected and replaced with a single underscore. Multiple separators are normalized, and leading/trailing underscores are removed.

## [v0.1.0] - 2025-06-27

- add: `contains`  
  Checks if a substring is present in the string. Operates on bytes, not code points.

- add: `has_prefix`  
  Checks if the string starts with the given prefix. Operates on bytes, not code points.

- add: `has_suffix`  
  Checks if the string ends with the given suffix. Operates on bytes, not code points.

- add: `contains_any`  
  Checks if any Unicode code point in the given set is present in the string. Unicode-aware.

- add: `count_substring`  
  Counts the number of non-overlapping occurrences of a substring in the string. Operates on bytes, not code points.

- add: `equal_fold`  
  Checks if two strings are equal, ignoring ASCII case. (Full Unicode case folding not yet supported.)

- add: `fields`  
  Splits a string by runs of Unicode whitespace, returning a list of substrings. Returns an empty list if only whitespace.

- add: `fields_func`  
  Splits a string at runs of Unicode code points where the given predicate returns true, returning a list of substrings.

- add: `index`  
  Returns the byte offset of the first occurrence of a substring in the string, or -1 if not found.

- add: `repeat`  
  Returns a new string consisting of the given string repeated a specified number of times. Raises an exception if the count is negative.

- add: `join`  
  Concatenates a list of strings, inserting the given separator between each element. Returns the empty string if the list is empty.
