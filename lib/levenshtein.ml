(** [distance s t] computes the Levenshtein edit distance between two UTF-8
    encoded strings. This function correctly handles multibyte characters,
    including Japanese, Chinese, emoji, and other Unicode symbols.

    The edit distance is the minimum number of single-character edits
    (insertions, deletions, or substitutions) required to change one string into
    the other.

    Example:
    - distance "kitten" "sitting" = 3
    - distance "こんにちは" "こんばんは" = 2
    - distance "🍎" "🍏" = 1 *)
let decode_utf8_array (s : string) : Uchar.t array =
  (* Decodes a UTF-8 encoded string [s] into an array of Unicode code points
     (Uchar.t). OCaml's [String.length] returns byte length, not character
     count, so we use the [Uutf] library to properly decode multibyte
     characters. *)
  let rec aux acc decoder =
    match Uutf.decode decoder with
    | `Uchar u ->
        aux (u :: acc)
          decoder (* Append valid Unicode character to accumulator *)
    | `End ->
        Array.of_list (List.rev acc)
        (* Reverse and convert list to array at end *)
    | `Malformed _ ->
        aux (Uchar.of_char '?' :: acc)
          decoder (* Replace malformed bytes with '?' *)
    | `Await ->
        assert false (* `Await` won't occur when decoding from a string *)
  in
  aux [] (Uutf.decoder (`String s))

let distance ~(s : string) ~(t : string) : int =
  (* Convert UTF-8 strings to arrays of Unicode code points *)
  let s_chars = decode_utf8_array s in
  let t_chars = decode_utf8_array t in

  let m = Array.length s_chars in
  let n = Array.length t_chars in

  (* Initialize DP (dynamic programming) matrix: dp.(i).(j) represents the edit
     distance between the first [i] characters of [s] and the first [j]
     characters of [t] *)
  let dp = Array.make_matrix (m + 1) (n + 1) 0 in

  (* Base cases: transforming to/from an empty string *)
  for i = 0 to m do
    dp.(i).(0) <- i
  done;
  for j = 0 to n do
    dp.(0).(j) <- j
  done;

  (* Main DP loop to compute minimal edit distance *)
  for i = 1 to m do
    for j = 1 to n do
      let cost = if Uchar.equal s_chars.(i - 1) t_chars.(j - 1) then 0 else 1 in
      dp.(i).(j) <-
        min
          (min (dp.(i - 1).(j) + 1) (* Deletion *) (dp.(i).(j - 1) + 1))
          (* Insertion *)
          (dp.(i - 1).(j - 1) + cost)
      (* Substitution *)
    done
  done;

  (* The final cell contains the edit distance between the full strings *)
  dp.(m).(n)
