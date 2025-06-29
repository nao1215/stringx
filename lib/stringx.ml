module Levenshtein = Levenshtein
open Uutf
open Stdlib

(** [utf8_length s] returns the number of Unicode code points in UTF-8 string
    [s]. This counts characters correctly, including multibyte characters like
    Japanese or emojis. *)
let utf8_length s =
  let dec = Uutf.decoder ~encoding:`UTF_8 (`String s) in
  let rec loop acc =
    match Uutf.decode dec with
    | `End -> acc
    | `Uchar _ -> loop (acc + 1)
    | `Malformed _ -> loop (acc + 1)
    | `Await -> acc
  in
  loop 0

(* [repeat_utf8 pad n] repeats the UTF-8 string [pad] [n] times. This function
   does not truncate to exact code point count. *)
let repeat_utf8 pad n =
  let rec loop acc i = if i = 0 then acc else loop (acc ^ pad) (i - 1) in
  loop "" n

(* [take_utf8 s n] takes the first [n] Unicode code points from UTF-8 string
   [s]. Invalid sequences are replaced with "?". *)
let take_utf8 s n =
  let dec = Uutf.decoder ~encoding:`UTF_8 (`String s) in
  let b = Buffer.create (String.length s) in
  let rec loop i =
    if i >= n then ()
    else
      match Uutf.decode dec with
      | `Uchar u ->
          Buffer.add_utf_8_uchar b u;
          loop (i + 1)
      | `End | `Await -> ()
      | `Malformed _ ->
          Buffer.add_string b "?";
          loop (i + 1)
  in
  loop 0;
  Buffer.contents b

(* [times_needed count pad_len] computes how many repetitions of
   [pad_len]-length padding are needed to cover at least [count] characters. *)
let times_needed count pad_len = ((count + pad_len - 1) / pad_len) + 1

(** [center s len pad] centers string [s] in a field of [len] characters,
    padding with [pad] on both sides. If [s] is already [len] or longer, it is
    returned unchanged. If [pad] is empty, [s] is returned. This function is
    Unicode-aware. *)
let center s len pad =
  if pad = "" then s
  else
    let slen = utf8_length s in
    if slen >= len then s
    else
      let total_pad = len - slen in
      let left_pad = total_pad / 2 in
      let right_pad = total_pad - left_pad in
      let pad_len = utf8_length pad in
      let left_full = repeat_utf8 pad (times_needed left_pad pad_len) in
      let right_full = repeat_utf8 pad (times_needed right_pad pad_len) in
      let left = take_utf8 left_full left_pad in
      let right = take_utf8 right_full right_pad in
      left ^ s ^ right

(** Decode a UTF-8 string into a list of Unicode characters (Uchar.t). *)
let decode_utf8 (s : string) : Uchar.t list =
  let dec = decoder ~encoding:`UTF_8 (`String s) in
  let rec loop acc =
    match decode dec with
    | `Uchar u -> loop (u :: acc)
    | `End -> List.rev acc
    | `Malformed _ -> loop acc
    | `Await -> acc
  in
  loop []

(** Encode a list of Unicode characters (Uchar.t) into a UTF-8 string. *)
let encode_utf8 (uchars : Uchar.t list) : string =
  let buf = Buffer.create 64 in
  let enc = encoder `UTF_8 (`Buffer buf) in
  let () =
    List.iter (fun u -> ignore (encode enc (`Uchar u))) uchars;
    ignore (encode enc `End)
  in
  Buffer.contents buf

(** Expand a character range from [start] to [stop], inclusive. *)
let expand_range (start : Uchar.t) (stop : Uchar.t) : Uchar.t list =
  let s = Uchar.to_int start in
  let e = Uchar.to_int stop in
  let low, high = if s <= e then (s, e) else (e, s) in
  List.init (high - low + 1) (fun i -> Uchar.of_int (low + i))

(** Parse the pattern string and build a matcher function. Supports:
    - direct characters: "aeiou"
    - ranges: "a-k"
    - negation: "^a-k" *)
let build_matcher (pattern : string) : Uchar.t -> bool =
  let uchars = decode_utf8 pattern in
  let is_negated =
    match uchars with
    | u :: _ when Uchar.equal u (Uchar.of_char '^') -> true
    | _ -> false
  in
  let uchars = if is_negated then List.tl uchars else uchars in
  let rec parse acc = function
    | [] -> acc
    | a :: b :: c :: tl when Uchar.equal b (Uchar.of_char '-') ->
        let range = expand_range a c in
        parse (range @ acc) tl
    | ch :: tl -> parse (ch :: acc) tl
  in
  let matcher_list = parse [] uchars in
  if is_negated then fun u -> not (List.mem u matcher_list)
  else fun u -> List.mem u matcher_list

(** Count how many Unicode characters in [str] match the given [pattern]. *)
let count (str : string) (pattern : string) : int =
  let matcher = build_matcher pattern in
  let dec = decoder ~encoding:`UTF_8 (`String str) in
  let rec loop acc =
    match decode dec with
    | `Uchar u -> if matcher u then loop (acc + 1) else loop acc
    | `End -> acc
    | `Malformed _ -> loop acc
    | `Await -> acc
  in
  loop 0

(** [delete str pattern] returns a new string in which all characters in [str]
    that match the given [pattern] are removed.

    The [pattern] follows the same syntax as used in [count] and [translate],
    and supports:
    - character sets: e.g., "aeiou"
    - ranges: e.g., "a-k"
    - negation with ^: e.g., "^a-k"

    Unicode-aware: input string is treated as a sequence of UTF-8 characters.

    Examples:
    - [delete "hello" "aeiou"] returns ["hll"]
    - [delete "hello" "a-k"] returns ["llo"]
    - [delete "hello" "^a-k"] returns ["he"]

    @param str The UTF-8 encoded input string
    @param pattern The character pattern to match
    @return A new string with matched characters removed *)
let delete (str : string) (pattern : string) : string =
  let matcher = build_matcher pattern in
  decode_utf8 str |> List.filter (fun u -> not (matcher u)) |> encode_utf8

(* [len str] returns the number of Unicode code points (runes) in UTF-8 string
   [str]. *)
let len (str : string) : int =
  let dec = Uutf.decoder ~encoding:`UTF_8 (`String str) in
  let rec loop acc =
    match Uutf.decode dec with
    | `End -> acc
    | `Uchar _ -> loop (acc + 1)
    | `Malformed _ -> loop (acc + 1)
    | `Await -> acc
  in
  loop 0

(** [reverse s] reverses a UTF-8 encoded string [s]. *)
let reverse (s : string) : string = decode_utf8 s |> List.rev |> encode_utf8

(** [contains s substr] reports whether [substr] is within [s]. Returns true if
    [substr] is the empty string. *)
let contains (s : string) (substr : string) : bool =
  if substr = "" then true
  else
    let len_s = String.length s in
    let len_sub = String.length substr in
    let rec loop i =
      if i > len_s - len_sub then false
      else if String.sub s i len_sub = substr then true
      else loop (i + 1)
    in
    loop 0

(** [has_prefix s prefix] checks if the string [s] starts with the given
    [prefix]. Returns true if [prefix] is empty. This function operates on
    bytes, not Unicode code points. *)
let has_prefix (s : string) (prefix : string) : bool =
  let len_s = String.length s in
  let len_p = String.length prefix in
  if len_p = 0 then true
  else if len_s < len_p then false
  else String.sub s 0 len_p = prefix

(** [has_suffix s suffix] reports whether the string [s] ends with [suffix].
    Returns true if [suffix] is the empty string. This function is
    Unicode-agnostic and operates on bytes, not code points. *)
let has_suffix (s : string) (suffix : string) : bool =
  let len_s = String.length s in
  let len_suf = String.length suffix in
  if len_suf = 0 then true
  else if len_s < len_suf then false
  else String.sub s (len_s - len_suf) len_suf = suffix

(** [contains_any s chars] reports whether any Unicode code points in [chars]
    are within [s]. Returns false if [chars] is empty. Unicode-aware. *)
let contains_any (s : string) (chars : string) : bool =
  if chars = "" then false
  else
    let set = decode_utf8 chars |> List.sort_uniq Uchar.compare in
    let rec loop = function
      | [] -> false
      | u :: tl -> if List.mem u set then true else loop tl
    in
    loop (decode_utf8 s)

(** [count_substring s substr] counts the number of non-overlapping instances of
    [substr] in [s]. If [substr] is the empty string, returns 1 + the number of
    Unicode code points in [s]. This function is Unicode-agnostic and operates
    on bytes, not code points. *)
let count_substring (s : string) (substr : string) : int =
  if substr = "" then 1 + len s
  else
    let len_s = String.length s in
    let len_sub = String.length substr in
    let rec loop i acc =
      if i > len_s - len_sub then acc
      else if String.sub s i len_sub = substr then loop (i + len_sub) (acc + 1)
      else loop (i + 1) acc
    in
    loop 0 0

(** [equal_fold s t] reports whether [s] and [t], interpreted as UTF-8 strings,
    are equal under simple Unicode case-folding (ASCII only). *)
let equal_fold (s : string) (t : string) : bool =
  let to_simple_fold str =
    decode_utf8 str
    |> List.map (fun u ->
           let c = Uchar.to_int u in
           (* Simple ASCII case folding *)
           if c >= 0x41 && c <= 0x5A then Uchar.of_int (c + 0x20) else u)
    |> encode_utf8
  in
  to_simple_fold s = to_simple_fold t

(** [is_space u] returns true if the Unicode character [u] is a whitespace
    character. *)
let is_space (u : Uchar.t) : bool =
  match Uchar.to_int u with
  | 0x09 | 0x0A | 0x0B | 0x0C | 0x0D | 0x20 | 0x85 | 0xA0 | 0x1680 | 0x2000
  | 0x2001 | 0x2002 | 0x2003 | 0x2004 | 0x2005 | 0x2006 | 0x2007 | 0x2008
  | 0x2009 | 0x200A | 0x2028 | 0x2029 | 0x202F | 0x205F | 0x3000 ->
      true
  | _ -> false

(** [fields s] splits [s] into substrings separated by one or more Unicode
    whitespace characters. Returns an empty list if [s] contains only
    whitespace. *)
let fields (s : string) : string list =
  let uchars = decode_utf8 s in
  let rec skip_spaces = function
    | u :: tl when is_space u -> skip_spaces tl
    | rest -> rest
  in
  let rec take_word acc = function
    | u :: tl when not (is_space u) -> take_word (u :: acc) tl
    | rest -> (List.rev acc, rest)
  in
  let rec loop acc l =
    match skip_spaces l with
    | [] -> List.rev acc
    | l' ->
        let word, rest = take_word [] l' in
        if word = [] then List.rev acc else loop (encode_utf8 word :: acc) rest
  in
  loop [] uchars

(** [fields_func s f] splits [s] at each run of Unicode code points [c]
    satisfying [f c]. Returns an empty list if all code points in [s] satisfy
    [f] or [s] is empty. *)
let fields_func (s : string) (f : Uchar.t -> bool) : string list =
  let uchars = decode_utf8 s in
  let rec skip_sep = function
    | u :: tl when f u -> skip_sep tl
    | rest -> rest
  in
  let rec take_field acc = function
    | u :: tl when not (f u) -> take_field (u :: acc) tl
    | rest -> (List.rev acc, rest)
  in
  let rec loop acc l =
    match skip_sep l with
    | [] -> List.rev acc
    | l' ->
        let word, rest = take_field [] l' in
        if word = [] then List.rev acc else loop (encode_utf8 word :: acc) rest
  in
  loop [] uchars

(** [index s substr] returns the index of the first instance of [substr] in [s],
    or -1 if [substr] is not present. The index is a byte offset (not code point
    index). *)
let index (s : string) (substr : string) : int =
  let len_s = String.length s in
  let len_sub = String.length substr in
  if substr = "" then 0
  else if len_sub > len_s then -1
  else
    let rec loop i =
      if i > len_s - len_sub then -1
      else if String.sub s i len_sub = substr then i
      else loop (i + 1)
    in
    loop 0

(** [repeat s count] returns a new string consisting of [count] copies of [s].
    Raises [Invalid_argument] if [count] is negative. *)
let repeat (s : string) (count : int) : string =
  if count < 0 then invalid_arg "repeat: negative count"
  else
    let rec loop acc n = if n = 0 then acc else loop (acc ^ s) (n - 1) in
    loop "" count

(** [join elems sep] concatenates the elements of [elems], inserting [sep]
    between each element. Returns the empty string if [elems] is empty. *)
let join (elems : string list) (sep : string) : string =
  match elems with
  | [] -> ""
  | hd :: tl -> List.fold_left (fun acc s -> acc ^ sep ^ s) hd tl

(** [trim s cutset] returns [s] with all leading and trailing Unicode code
    points contained in [cutset] removed. Unicode-aware. *)
let trim (s : string) (cutset : string) : string =
  if cutset = "" || s = "" then s
  else
    let set = decode_utf8 cutset |> List.sort_uniq Uchar.compare in
    let uchars = decode_utf8 s in
    let rec drop_leading = function
      | u :: tl when List.mem u set -> drop_leading tl
      | rest -> rest
    in
    let rec drop_trailing l =
      match List.rev l with
      | u :: tl when List.mem u set -> drop_trailing (List.rev tl)
      | _ -> l
    in
    drop_leading uchars |> drop_trailing |> encode_utf8

(** [trim_func s f] returns [s] with all leading and trailing Unicode code
    points [c] satisfying [f c] removed. Unicode-aware. *)
let trim_func (s : string) (f : Uchar.t -> bool) : string =
  if s = "" then s
  else
    let uchars = decode_utf8 s in
    let rec drop_leading = function
      | u :: tl when f u -> drop_leading tl
      | rest -> rest
    in
    let rec drop_trailing l =
      match List.rev l with
      | u :: tl when f u -> drop_trailing (List.rev tl)
      | _ -> l
    in
    drop_leading uchars |> drop_trailing |> encode_utf8

(** [trim_left s cutset] returns [s] with all leading Unicode code points
    contained in [cutset] removed. Unicode-aware. *)
let trim_left (s : string) (cutset : string) : string =
  if cutset = "" || s = "" then s
  else
    let set = decode_utf8 cutset |> List.sort_uniq Uchar.compare in
    let uchars = decode_utf8 s in
    let rec drop_leading = function
      | u :: tl when List.mem u set -> drop_leading tl
      | rest -> rest
    in
    drop_leading uchars |> encode_utf8

(** [trim_left_func s f] returns [s] with all leading Unicode code points [c]
    satisfying [f c] removed. Unicode-aware. *)
let trim_left_func (s : string) (f : Uchar.t -> bool) : string =
  if s = "" then s
  else
    let uchars = decode_utf8 s in
    let rec drop_leading = function
      | u :: tl when f u -> drop_leading tl
      | rest -> rest
    in
    drop_leading uchars |> encode_utf8

(** [trim_right s cutset] returns [s] with all trailing Unicode code points
    contained in [cutset] removed. Unicode-aware. *)
let trim_right (s : string) (cutset : string) : string =
  if cutset = "" || s = "" then s
  else
    let set = decode_utf8 cutset |> List.sort_uniq Uchar.compare in
    let uchars = decode_utf8 s in
    let rec drop_trailing l =
      match List.rev l with
      | u :: tl when List.mem u set -> drop_trailing (List.rev tl)
      | _ -> l
    in
    drop_trailing uchars |> encode_utf8

(** [trim_right_func s f] returns [s] with all trailing Unicode code points [c]
    satisfying [f c] removed. Unicode-aware. *)
let trim_right_func (s : string) (f : Uchar.t -> bool) : string =
  if s = "" then s
  else
    let uchars = decode_utf8 s in
    let rec drop_trailing l =
      match List.rev l with
      | u :: tl when f u -> drop_trailing (List.rev tl)
      | _ -> l
    in
    drop_trailing uchars |> encode_utf8

(** [trim_space s] returns [s] with all leading and trailing Unicode whitespace
    removed. Unicode-aware. *)
let trim_space (s : string) : string =
  let is_space = is_space in
  if s = "" then s
  else
    let uchars = decode_utf8 s in
    let rec drop_leading = function
      | u :: tl when is_space u -> drop_leading tl
      | rest -> rest
    in
    let rec drop_trailing l =
      match List.rev l with
      | u :: tl when is_space u -> drop_trailing (List.rev tl)
      | _ -> l
    in
    drop_leading uchars |> drop_trailing |> encode_utf8

(** [trim_suffix s suffix] returns [s] without the provided trailing [suffix]
    string. If [s] does not end with [suffix], [s] is returned unchanged. This
    function is byte-based, not Unicode-aware. *)
let trim_suffix (s : string) (suffix : string) : string =
  let len_s = String.length s in
  let len_suf = String.length suffix in
  if len_suf = 0 then s
  else if len_s < len_suf then s
  else if String.sub s (len_s - len_suf) len_suf = suffix then
    String.sub s 0 (len_s - len_suf)
  else s

(** [to_lower s] returns [s] with all Unicode letters mapped to their lower
    case. This function is ASCII-only for now. *)
let to_lower (s : string) : string =
  let lower u =
    let c = Uchar.to_int u in
    if c >= 0x41 && c <= 0x5A then Uchar.of_int (c + 0x20) else u
  in
  decode_utf8 s |> List.map lower |> encode_utf8

(** [to_title s] returns [s] with all Unicode letters mapped to their Unicode
    title case. Currently, only ASCII letters are supported (A-Z, a-z). TODO:
    Support full Unicode title case in the future. *)
let to_title (s : string) : string =
  let title u =
    let c = Uchar.to_int u in
    if c >= 0x61 && c <= 0x7A then Uchar.of_int (c - 0x20) (* a-z -> A-Z *)
    else if c >= 0x41 && c <= 0x5A then u (* already uppercase *)
    else u
  in
  decode_utf8 s |> List.map title |> encode_utf8

(** [to_upper s] returns [s] with all Unicode letters mapped to their upper
    case. This function is ASCII-only for now. *)
let to_upper (s : string) : string =
  let upper u =
    let c = Uchar.to_int u in
    if c >= 0x61 && c <= 0x7A then Uchar.of_int (c - 0x20) else u
  in
  decode_utf8 s |> List.map upper |> encode_utf8

(** [to_camel_case s] converts words separated by space, underscore, or hyphen
    to camel case. Leading and trailing underscores are preserved. Multiple
    consecutive separators are treated as a single word boundary. If there are
    no separators, the string is returned unchanged.

    - Words are split on '_', '-', or space.
    - The first word is lowercased (even if originally all uppercase).
    - Subsequent words are capitalized (first letter uppercase, rest lowercase).
    - All-uppercase words are handled (e.g. "GOLANG_IS_GREAT" →
      "golangIsGreat").
    - If there are no separators, the original string is returned (e.g.
      "alreadyCamel" → "alreadyCamel").
    - Leading and trailing underscores are preserved (e.g. "_complex__case_" →
      "_complexCase_").
    - Hyphens and spaces are also treated as word boundaries.

    Examples:
    - [to_camel_case "some_words"] returns ["someWords"]
    - [to_camel_case "_complex__case_"] returns ["_complexCase_"]
    - [to_camel_case "GOLANG_IS_GREAT"] returns ["golangIsGreat"]
    - [to_camel_case "alreadyCamel"] returns ["alreadyCamel"]
    - [to_camel_case "foo-BarBaz"] returns ["fooBarBaz"]
    - [to_camel_case "word"] returns ["word"]
    - [to_camel_case ""] returns [""] *)
let to_camel_case (s : string) : string =
  let is_sep c = c = '_' || c = '-' || c = ' ' in
  let len = String.length s in

  (* count leading '_' *)
  let rec count_lead i =
    if i < len && s.[i] = '_' then 1 + count_lead (i + 1) else 0
  in
  let lead = count_lead 0 in

  (* count trailing '_' *)
  let rec count_trail i =
    if i >= 0 && s.[i] = '_' then 1 + count_trail (i - 1) else 0
  in
  let trail = count_trail (len - 1) in

  let core_start = lead in
  let core_len = len - lead - trail in
  let core = if core_len > 0 then String.sub s core_start core_len else "" in

  (* helper: split on runs of separators *)
  let split_core str =
    let n = String.length str in
    let rec aux i acc =
      if i >= n then List.rev acc
      else if is_sep str.[i] then aux (i + 1) acc
      else
        let j = ref i in
        while !j < n && not (is_sep str.[!j]) do
          incr j
        done;
        let word = String.sub str i (!j - i) in
        aux !j (word :: acc)
    in
    aux 0 []
  in

  (* helper: check all-uppercase ASCII *)
  let is_all_upper seg =
    seg <> "" && String.for_all (fun c -> 'A' <= c && c <= 'Z') seg
  in

  let buf = Buffer.create len in

  (* add leading underscores *)
  for _ = 1 to lead do
    Buffer.add_char buf '_'
  done;

  (if core = "" then () (* nothing to add *)
   else if not (String.exists (fun c -> is_sep c) core) then
     (* no separators: keep original *)
     Buffer.add_string buf core
   else
     let segments = split_core core in
     match segments with
     | [] -> ()
     | first :: rest ->
         (* first segment: all lowercase *)
         Buffer.add_string buf (String.lowercase_ascii first);
         List.iter
           (fun seg ->
             if seg = "" then ()
             else if is_all_upper seg then (
               let low = String.lowercase_ascii seg in
               let c0 = low.[0] in
               Buffer.add_char buf (Char.uppercase_ascii c0);
               if String.length low > 1 then
                 Buffer.add_string buf
                   (String.sub low 1 (String.length low - 1)))
             else
               (* mixed-case or lowercase: preserve except first char *)
               let c0 = seg.[0] in
               Buffer.add_char buf (Char.uppercase_ascii c0);
               if String.length seg > 1 then
                 Buffer.add_string buf
                   (String.sub seg 1 (String.length seg - 1)))
           rest);

  (* add trailing underscores *)
  for _ = 1 to trail do
    Buffer.add_char buf '_'
  done;

  Buffer.contents buf
