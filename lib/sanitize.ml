(* Output filters shared by the chat clients. *)

(* Chat text is attacker-controlled, so printed lines go through these
   filters. Replaced characters are shown as ⟨U+XXXX⟩, which is hard to
   confuse with someone literally typing "\x1b". *)

let add_marker b cp = Printf.bprintf b "⟨U+%04X⟩" cp

(* Characters that can drive the terminal: C0 controls (incl. ESC), DEL, and
   C1 controls (U+0080..U+009F, UTF-8 encoded as C2 80..C2 9F), which some
   terminals also interpret. *)
let sanitize_controls s =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let rec go i =
    if i < n then
      match Char.code s.[i] with
      | c when c < 0x20 || c = 0x7f ->
          add_marker b c;
          go (i + 1)
      | 0xc2 when i + 1 < n && Char.code s.[i + 1] land 0xe0 = 0x80 ->
          add_marker b (Char.code s.[i + 1]);
          go (i + 2)
      | _ ->
          Buffer.add_char b s.[i];
          go (i + 1)
  in
  go 0;
  Buffer.contents b

(* Characters that can make a message look like something else: line and
   paragraph separators (fake extra lines) and bidi embeddings, overrides and
   isolates (reordered text). All are 3-byte UTF-8 sequences starting E2. *)
let sanitize_spoofing s =
  let is_spoofing cp =
    cp = 0x2028 || cp = 0x2029
    || (cp >= 0x202A && cp <= 0x202E)
    || (cp >= 0x2066 && cp <= 0x2069)
  in
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let rec go i =
    if i + 2 < n && s.[i] = '\xe2' then
      let cp =
        0x2000
        lor ((Char.code s.[i + 1] land 0x3f) lsl 6)
        lor (Char.code s.[i + 2] land 0x3f)
      in
      if is_spoofing cp then (
        add_marker b cp;
        go (i + 3))
      else (
        Buffer.add_char b s.[i];
        go (i + 1))
    else if i < n then (
      Buffer.add_char b s.[i];
      go (i + 1))
  in
  go 0;
  Buffer.contents b

(* Applied to every printed line, in order. Remove an entry to disable it. *)
let output_filters = [ sanitize_controls; sanitize_spoofing ]

(* [(|>)] is exactly fold_left's (accumulator -> element) callback here:
   the line so far, piped into the next filter. *)
let apply_filters : string -> (string -> string) list -> string =
  List.fold_left ( |> )
