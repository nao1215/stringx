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
    if len_sub = 0 then true else loop 0

(** [has_prefix s prefix] checks if the string [s] starts with the given
    [prefix]. Returns true if [prefix] is empty. This function is Unicode-aware,
    meaning it correctly handles multibyte characters like Japanese or emojis.
*)
let has_prefix (s : string) (prefix : string) : bool =
  let len_s = String.length s in
  let len_p = String.length prefix in
  if len_p = 0 then true
  else if len_s < len_p then false
  else String.sub s 0 len_p = prefix
