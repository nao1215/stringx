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

(** [to_kebab_case s] converts a string to kebab-case.
    - Uppercase ASCII letters are converted to lowercase.
    - Word boundaries are detected at transitions from lowercase to uppercase,
      from letter to digit, and at underscores, spaces, or hyphens.
    - All word boundaries are replaced with a single hyphen '-'.
    - Multiple consecutive separators are treated as a single hyphen.
    - Leading and trailing hyphens are removed.
    - If the input is empty, returns the empty string.

    Examples:
    - [to_kebab_case "FirstName"] returns ["first-name"]
    - [to_kebab_case "HTTPServer"] returns ["http-server"]
    - [to_kebab_case "NoHTTPS"] returns ["no-https"]
    - [to_kebab_case "GO_PATH"] returns ["go-path"]
    - [to_kebab_case "GO PATH"] returns ["go-path"]
    - [to_kebab_case "GO-PATH"] returns ["go-path"]
    - [to_kebab_case "http2xx"] returns ["http-2xx"]
    - [to_kebab_case "HTTP20xOK"] returns ["http-20x-ok"]
    - [to_kebab_case "Duration2m3s"] returns ["duration-2m-3s"]
    - [to_kebab_case "Bld4Floor3rd"] returns ["bld4-floor-3rd"] *)
let to_kebab_case (s : string) : string =
  (* Character classification helpers *)
  let is_lower c = 'a' <= c && c <= 'z' in
  let is_upper c = 'A' <= c && c <= 'Z' in
  let is_digit c = '0' <= c && c <= '9' in
  let is_sep c = c = '_' || c = '-' || c = ' ' in

  let len = String.length s in
  (* Step 1: Split the string into raw segments at logical boundaries *)
  let raw_segs = ref [] in
  let i = ref 0 in
  while !i < len do
    (* Skip any separator characters *)
    while !i < len && is_sep s.[!i] do
      incr i
    done;
    if !i < len then (
      let start = !i in
      incr i;
      (* Advance until next separator or case/digit boundary *)
      while
        !i < len
        && (not (is_sep s.[!i]))
        && not
             (let prev = s.[!i - 1] in
              let curr = s.[!i] in
              let next_opt = if !i + 1 < len then Some s.[!i + 1] else None in
              (* lower→upper transition *)
              (is_lower prev && is_upper curr)
              (* acronym boundary: upper→upper followed by lower *)
              || (is_upper prev && is_upper curr
                 && match next_opt with Some c2 -> is_lower c2 | None -> false)
              (* letter→digit *)
              || ((is_lower prev || is_upper prev) && is_digit curr)
              (* digit→upper *)
              || (is_digit prev && is_upper curr))
      do
        incr i
      done;
      let seg = String.sub s start (!i - start) in
      raw_segs := seg :: !raw_segs)
  done;
  let segs = Array.of_list (List.rev !raw_segs) in

  (* Step 2: Merge purely numeric segments with adjacent segments to avoid
     isolated numbers *)
  let n = Array.length segs in
  let merged = ref [] in
  let j = ref 0 in
  while !j < n do
    let seg = segs.(!j) in
    let only_digits = seg <> "" && String.for_all (fun c -> is_digit c) seg in
    if only_digits && !j > 0 && !j < n - 1 then
      let next = segs.(!j + 1) in
      if next <> "" && is_upper next.[0] then (
        (* If next segment starts with uppercase, attach number to previous *)
        let prev = List.hd !merged in
        merged := List.tl !merged;
        merged := (prev ^ seg) :: !merged;
        incr j)
      else (
        (* Otherwise attach number to following segment *)
        segs.(!j + 1) <- seg ^ next;
        incr j)
    else (
      (* Keep segment as is *)
      merged := seg :: !merged;
      incr j)
  done;
  let final = List.rev !merged in

  (* Step 3: Lowercase everything and join with hyphens *)
  String.concat "-" (List.map String.lowercase_ascii final)

(** [to_pascal_case s] converts words separated by space, underscore, or hyphen
    to PascalCase.
    - Words are split on '_', '-', or space.
    - Each word is capitalized (first letter uppercase, rest lowercase).
    - All-uppercase words are handled (e.g. "OCAML_IS_GREAT" → "OcamlIsGreat").
    - If there are no separators, the first letter is uppercased, the rest
      lowercased.
    - Leading and trailing underscores are preserved (e.g. "_complex__case_" →
      "_ComplexCase_").
    - Multiple consecutive separators are preserved as single underscores.
    - Hyphens and spaces are also treated as word boundaries.

    Examples:
    - [to_pascal_case "some_words"] returns ["SomeWords"]
    - [to_pascal_case "_complex__case_"] returns ["_ComplexCase_"]
    - [to_pascal_case "GOLANG_IS_GREAT"] returns ["GolangIsGreat"]
    - [to_pascal_case "alreadyPascal"] returns ["AlreadyPascal"]
    - [to_pascal_case "foo-BarBaz"] returns ["FooBarBaz"]
    - [to_pascal_case "word"] returns ["Word"]
    - [to_pascal_case ""] returns [""] *)
let to_pascal_case (s : string) : string =
  let is_sep c = c = '_' || c = '-' || c = ' ' in
  let is_upper c = 'A' <= c && c <= 'Z' in
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
  let core_end = len - trail in

  let buf = Buffer.create len in
  (* add leading underscores *)
  for _ = 1 to lead do
    Buffer.add_char buf '_'
  done;

  (if core_start < core_end then
     let core = String.sub s core_start (core_end - core_start) in
     (* check for any separator in core *)
     let has_sep = String.exists (fun c -> is_sep c) core in

     if not has_sep then (
       (* no separators: uppercase first char, keep rest as-is *)
       Buffer.add_char buf (Char.uppercase_ascii core.[0]);
       if String.length core > 1 then
         Buffer.add_substring buf core 1 (String.length core - 1))
     else
       (* split on separators, ignore empty segments *)
       let rec split i acc =
         if i >= String.length core then List.rev acc
         else if is_sep core.[i] then split (i + 1) acc
         else
           let j = ref i in
           while !j < String.length core && not (is_sep core.[!j]) do
             incr j
           done;
           let seg = String.sub core i (!j - i) in
           split !j (seg :: acc)
       in
       let segments = split 0 [] in

       List.iter
         (fun seg ->
           if seg <> "" then
             (* determine if all-uppercase acronym *)
             let all_upper = seg <> "" && String.for_all is_upper seg in
             if all_upper then (
               let low = String.lowercase_ascii seg in
               Buffer.add_char buf (Char.uppercase_ascii low.[0]);
               if String.length low > 1 then
                 Buffer.add_substring buf low 1 (String.length low - 1))
             else (
               (* mixed-case or lowercase: uppercase first, keep rest *)
               Buffer.add_char buf (Char.uppercase_ascii seg.[0]);
               if String.length seg > 1 then
                 Buffer.add_substring buf seg 1 (String.length seg - 1)))
         segments);

  (* add trailing underscores *)
  for _ = 1 to trail do
    Buffer.add_char buf '_'
  done;

  Buffer.contents buf

(** [to_snake_case s] converts a string to snake_case.
    - Uppercase ASCII letters are converted to lowercase.
    - Word boundaries are detected at transitions from lowercase to uppercase,
      from letter to digit, and at underscores, spaces, or hyphens.
    - All word boundaries are replaced with a single underscore '_'.
    - Multiple consecutive separators are treated as a single underscore.
    - Leading and trailing underscores are removed.
    - If the input is empty, returns the empty string.

    Examples:
    - [to_snake_case "FirstName"] returns ["first_name"]
    - [to_snake_case "HTTPServer"] returns ["http_server"]
    - [to_snake_case "NoHTTPS"] returns ["no_https"]
    - [to_snake_case "GO_PATH"] returns ["go_path"]
    - [to_snake_case "GO PATH"] returns ["go_path"]
    - [to_snake_case "GO-PATH"] returns ["go_path"]
    - [to_snake_case "http2xx"] returns ["http_2xx"]
    - [to_snake_case "HTTP20xOK"] returns ["http_20x_ok"]
    - [to_snake_case "Duration2m3s"] returns ["duration_2m3s"]
    - [to_snake_case "Bld4Floor3rd"] returns ["bld4_floor_3rd"] *)
let to_snake_case (s : string) : string =
  let is_lower c = 'a' <= c && c <= 'z' in
  let is_upper c = 'A' <= c && c <= 'Z' in
  let is_digit c = '0' <= c && c <= '9' in
  let is_sep c = c = '_' || c = '-' || c = ' ' in

  let n = String.length s in
  let segments = ref [] in
  let i = ref 0 in

  (* 1. Segment the string *)
  while !i < n do
    if is_sep s.[!i] then incr i
    else
      let start = !i in
      let seg =
        if is_digit s.[!i] then (
          (* numeric group: digits + following lowercase letters *)
          let j = ref !i in
          while !j + 1 < n && (is_digit s.[!j + 1] || is_lower s.[!j + 1]) do
            incr j
          done;
          i := !j + 1;
          String.sub s start (!j - start + 1))
        else if is_upper s.[!i] then (
          (* uppercase: acronym or capitalized word *)
          let j = ref !i in
          while !j + 1 < n && is_upper s.[!j + 1] do
            incr j
          done;
          if !j > start && !j + 1 < n && is_lower s.[!j + 1] then (
            (* acronym boundary *)
            let acr_end = !j - 1 in
            i := acr_end + 1;
            String.sub s start (acr_end - start + 1))
          else if !j > start then (
            let len = !j - start + 1 in
            i := !j + 1;
            String.sub s start len)
          else
            (* single uppercase + lowers *)
            let k = ref !j in
            while !k + 1 < n && is_lower s.[!k + 1] do
              incr k
            done;
            i := !k + 1;
            String.sub s start (!k - start + 1))
        else if is_lower s.[!i] then (
          (* all-lowercase word *)
          let j = ref !i in
          while !j + 1 < n && is_lower s.[!j + 1] do
            incr j
          done;
          i := !j + 1;
          String.sub s start (!j - start + 1))
        else (
          (* any other character: return it as a single-char string *)
          incr i;
          String.make 1 s.[start])
      in
      if seg <> "" then segments := seg :: !segments
  done;

  (* 2. Merge single-digit segments into the previous word *)
  let merged = ref [] in
  List.iter
    (fun seg ->
      if
        String.length seg = 1
        && String.for_all (fun c -> is_digit c) seg
        && !merged <> []
      then (
        let prev = List.hd !merged in
        merged := List.tl !merged;
        merged := (prev ^ seg) :: !merged)
      else merged := seg :: !merged)
    (List.rev !segments);

  (* 3. Lowercase and join with '_' *)
  let final = List.rev !merged in
  String.concat "_" (List.map String.lowercase_ascii final)

(** [map f s] returns a new string which is the result of applying [f] to each
    Unicode code point of [s]. The mapping function [f] must return a valid
    [Uchar.t] for every input.

    This function is Unicode-aware: it decodes [s] into code points, applies
    [f], then re-encodes into UTF-8.

    Example: let rot13 u = let c = Uchar.to_int u in if c >= Char.code 'A' && c
    <= Char.code 'Z' then Uchar.of_int (Char.code 'A' + ((c - Char.code 'A' +
    13) mod 26)) else if c >= Char.code 'a' && c <= Char.code 'z' then
    Uchar.of_int (Char.code 'a' + ((c - Char.code 'a' + 13) mod 26)) else u in
    map rot13 "'Twas brillig and the slithy camel..." = "'Gjnf oevyyvt naq gur
    fyvgul pnzry..." *)
let map (f : Uchar.t -> Uchar.t) (s : string) : string =
  let dec = decoder ~encoding:`UTF_8 (`String s) in
  let buf = Buffer.create (String.length s) in
  let rec aux () =
    match decode dec with
    | `Uchar u ->
        let u' = f u in
        Buffer.add_utf_8_uchar buf u';
        aux ()
    | `Malformed _ -> aux ()
    | `End | `Await -> ()
  in
  aux ();
  Buffer.contents buf

(** [filter_map f s] applies [f] to each Unicode code point [u] of [s]. If [f u]
    returns [Some u'], [u'] is kept in the result; if [None], [u] is dropped.

    This function is Unicode-aware: it decodes [s] into code points, applies
    [f], then re-encodes into UTF-8.

    Example: let drop_vowel u = match Uchar.to_int u with | c when List.mem c
    [ Char.code 'a'; Char.code 'e'; Char.code 'i' ; Char.code 'o'; Char.code 'u'
     ] -> None | _ -> Some u in filter_map drop_vowel "hello" = "hll" *)
let filter_map (f : Uchar.t -> Uchar.t option) (s : string) : string =
  let dec = decoder ~encoding:`UTF_8 (`String s) in
  let buf = Buffer.create (String.length s) in
  let rec aux () =
    match decode dec with
    | `Uchar u ->
        (match f u with Some u' -> Buffer.add_utf_8_uchar buf u' | None -> ());
        aux ()
    | `Malformed _ -> aux ()
    | `End | `Await -> ()
  in
  aux ();
  Buffer.contents buf

(** [iter f s] applies [f] to each Unicode code point of [s], in sequence,
    purely for side-effects. *)
let iter (f : Uchar.t -> unit) (s : string) : unit =
  let dec = decoder ~encoding:`UTF_8 (`String s) in
  let rec aux () =
    match decode dec with
    | `Uchar u ->
        f u;
        aux ()
    | `Malformed _ ->
        (* ignore invalid sequences *)
        aux ()
    | `End | `Await -> ()
  in
  aux ()

(** [fold f init s] applies [f acc u] to each Unicode code point [u] of [s],
    carrying along an accumulator [acc], and returns the final accumulator. *)
let fold (f : 'acc -> Uchar.t -> 'acc) (init : 'acc) (s : string) : 'acc =
  let dec = decoder ~encoding:`UTF_8 (`String s) in
  let rec aux acc =
    match decode dec with
    | `Uchar u ->
        (* process code point and update accumulator *)
        aux (f acc u)
    | `Malformed _ ->
        (* skip invalid sequences *)
        aux acc
    | `End | `Await ->
        (* end of input, return accumulated result *)
        acc
  in
  aux init

let rune_width (u : Uchar.t) : int =
  let c = Uchar.to_int u in
  (* Basic CJK Unified Ideographs, Hangul, Hiragana, Katakana, etc. *)
  if
    (c >= 0x1100 && c <= 0x115F)
    || (c >= 0x2329 && c <= 0x232A)
    || (c >= 0x2E80 && c <= 0xA4CF)
    || (c >= 0xAC00 && c <= 0xD7A3)
    || (c >= 0xF900 && c <= 0xFAFF)
    || (c >= 0xFE10 && c <= 0xFE19)
    || (c >= 0xFE30 && c <= 0xFE6F)
    || (c >= 0xFF00 && c <= 0xFF60)
    || (c >= 0xFFE0 && c <= 0xFFE6)
    || (c >= 0x20000 && c <= 0x2FFFD)
    || (c >= 0x30000 && c <= 0x3FFFD)
  then 2
  else 1

(** [expand_tabs s tab_size] expands tab characters ('\t') in [s] to spaces,
    depending on the current column and [tab_size]. The column is reset to zero
    after each newline ('\n'). CJK characters are treated as width 2.

    Raises [Invalid_argument] if [tab_size] <= 0.

    Examples:
    - expand_tabs "a\tbc\tdef\tghij\tk" 4 = "a bc def ghij k"
    - expand_tabs "abcdefg\thij\nk\tl" 4 = "abcdefg hij\nk l"
    - expand_tabs "z中\t文\tw" 4 = "z中 文 w" *)
let expand_tabs (s : string) (tab_size : int) : string =
  if tab_size <= 0 then invalid_arg "expand_tabs: tab_size must be > 0";
  let dec = Uutf.decoder ~encoding:`UTF_8 (`String s) in
  let buf = Buffer.create (String.length s) in
  let rec aux col =
    match Uutf.decode dec with
    | `Uchar u ->
        if Uchar.to_int u = int_of_char '\n' then (
          Buffer.add_utf_8_uchar buf u;
          aux 0)
        else if Uchar.to_int u = int_of_char '\t' then (
          let spaces = tab_size - (col mod tab_size) in
          for _ = 1 to spaces do
            Buffer.add_char buf ' '
          done;
          aux (col + spaces))
        else
          let w = rune_width u in
          Buffer.add_utf_8_uchar buf u;
          aux (col + w)
    | `End | `Await -> ()
    | `Malformed _ -> aux (col + 1)
  in
  aux 0;
  Buffer.contents buf

(** [first_rune_to_lower s] returns [s] with the first Unicode code point
    converted to lower case if it is an uppercase ASCII letter. Unicode-aware:
    only the first code point is affected, the rest are unchanged.

    Examples:
    - first_rune_to_lower "CamelCase" = "camelCase"
    - first_rune_to_lower "camelCase" = "camelCase"
    - first_rune_to_lower "CAMEL" = "cAMEL"
    - first_rune_to_lower "こんにちは" = "こんにちは" *)
let first_rune_to_lower (s : string) : string =
  let dec = Uutf.decoder ~encoding:`UTF_8 (`String s) in
  let buf = Buffer.create (String.length s) in
  let rec aux first =
    match Uutf.decode dec with
    | `Uchar u when first ->
        let c = Uchar.to_int u in
        let u' =
          if c >= 0x41 && c <= 0x5A then Uchar.of_int (c + 0x20) else u
        in
        Buffer.add_utf_8_uchar buf u';
        aux false
    | `Uchar u ->
        Buffer.add_utf_8_uchar buf u;
        aux false
    | `End | `Await -> ()
    | `Malformed _ -> aux false
  in
  aux true;
  Buffer.contents buf

(** [first_rune_to_upper s] returns [s] with the first Unicode code point
    converted to upper case if it is a lowercase ASCII letter. Unicode-aware:
    only the first code point is affected, the rest are unchanged.

    Examples:
    - first_rune_to_upper "camelCase" = "CamelCase"
    - first_rune_to_upper "CamelCase" = "CamelCase"
    - first_rune_to_upper "camel" = "Camel"
    - first_rune_to_upper "こんにちは" = "こんにちは" *)
let first_rune_to_upper (s : string) : string =
  let dec = Uutf.decoder ~encoding:`UTF_8 (`String s) in
  let buf = Buffer.create (String.length s) in
  let rec aux first =
    match Uutf.decode dec with
    | `Uchar u when first ->
        let c = Uchar.to_int u in
        let u' =
          if c >= 0x61 && c <= 0x7A then Uchar.of_int (c - 0x20) else u
        in
        Buffer.add_utf_8_uchar buf u';
        aux false
    | `Uchar u ->
        Buffer.add_utf_8_uchar buf u;
        aux false
    | `End | `Await -> ()
    | `Malformed _ -> aux false
  in
  aux true;
  Buffer.contents buf
