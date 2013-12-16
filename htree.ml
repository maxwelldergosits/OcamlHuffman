

type htree = Leaf of char | Node of htree * htree

type bit = Zero | One

type htree_encoding = bit list

let to_char_list s =
  let rec to_char_list_h s l i =
    if i < 0 then l
    else to_char_list_h s (s.[i] :: l) (i-1) in
  to_char_list_h (s) ([]) ((String.length s) -1)

let count_chars (chars : char list) : ((char * int) list) =
  let table = Hashtbl.create 256 in
  let count_char c =
    if not (Hashtbl.mem table c) then
      Hashtbl.add table c 1
    else
      let n = Hashtbl.find table c in
      Hashtbl.replace table c (n+1) in
  List.iter (count_char) chars;
  Hashtbl.fold (fun k v acc -> (k,v)::acc) table []

let enqueue (l : (char * int) list) : htree =
  let leafs = List.map (fun (c,n) -> (n,Leaf(c))) l in
  let rec helper ns =
    match ns with
    | [] -> assert false
    | [_,h] -> h
    | (a,t)::(b,t1)::tl ->
      let node = a+b,Node(t,t1) in
      helper (List.merge (fun (x,_) (y,_) -> Pervasives.compare x y) tl [node]) in
  helper leafs

let make_tree (str : string)  =
  let chars = to_char_list str in
  let pairs = count_chars chars in
  let sorted = List.sort (fun (_,x) (_,y) -> Pervasives.compare x y) pairs in
  enqueue sorted,chars


let rec bitlist_to_byte_list bl : (char list) =
  let bl_to_byte (a,b,c,d,e,f,g,h) =
    let vb i = function Zero -> 0 | One -> i in
    let value = (vb 128 a) + (vb 64 b) + (vb 32 c) + (vb 16 d) + (vb 8 e) + (vb 4 f) + (vb 2 g) + (vb 1 h) in
    char_of_int value in
  match bl with
  | a::b::c::d::e::f::g::h::rest ->
    bl_to_byte (a,b,c,d,e,f,g,h) :: (bitlist_to_byte_list rest)
  | a::b::c::d::e::f::g::[] ->
    [bl_to_byte (a,b,c,d,e,f,g,Zero)]
  | a::b::c::d::e::f::[] ->
    [bl_to_byte (a,b,c,d,e,f,Zero,Zero)]
  | a::b::c::d::e::[] ->
    [bl_to_byte (a,b,c,d,e,Zero,Zero,Zero)]
  | a::b::c::d::[] ->
    [bl_to_byte (a,b,c,d,Zero,Zero,Zero,Zero)]
  | a::b::c::[] ->
    [bl_to_byte (a,b,c,Zero,Zero,Zero,Zero,Zero)]
  | a::b::[] ->
    [bl_to_byte (a,b,Zero,Zero,Zero,Zero,Zero,Zero)]
  | a::[] ->
    [bl_to_byte (a,Zero,Zero,Zero,Zero,Zero,Zero,Zero)]
  | [] -> []

let rec charlist_to_string bl =
  match bl with
  | h::t -> (String.make 1 h)^(charlist_to_string t)
  | [] -> ""

let encode_string (cl : char list) (h: htree) =
  let rec char_to_encoding_h c h0=
    match h0 with
    | Leaf(c0) -> if c0 = c then Some([]) else None
    | Node(l,r) ->
      match char_to_encoding_h c l, char_to_encoding_h c r with
      | Some(le),None -> Some(Zero::le)
      | None,Some(ri) -> Some(One::ri)
      | _ -> None in
  let f = (fun x -> match char_to_encoding_h x h with Some(e) -> e | None -> failwith "invalid tree") in
  let bits = List.flatten (List.map (f) cl) in
  let chars = bitlist_to_byte_list bits in
  charlist_to_string chars,(List.length bits)

let encode_all (h:htree) (l:int) (str:string) : (string) =
  Marshal.to_string (l,h,str) []

let read_string str :(int*htree*string) =
  Marshal.from_string str 0

let do_encode (str) =
  let (tree,cl) = make_tree str in
  let (en,l) = encode_string (cl) (tree) in
  encode_all tree l en

let read_whole_file filename =
  let file = open_in_bin filename in
  let size = in_channel_length file in
  let contents = String.create size in
  really_input file contents 0 size;
  close_in_noerr file;
  contents

let decode_bl bl tree =
  let rec get_char h0 bl =
    match bl with
    |Zero::t -> (match h0 with Leaf(c) -> c::(get_char tree bl) | Node(l,_) -> get_char l t)
    |One::t -> (match h0 with Leaf(c) -> c::(get_char tree bl) | Node(_,r) -> get_char r t)
    | [] -> [] in
  let cl = get_char tree bl in
  charlist_to_string cl

let string_to_bitlist length str : bit list =
  let cl = to_char_list str in
  let char_to_bitlist c =
    let value = int_of_char c in
    let a = if ((value lsr 7) mod 2) = 0 then Zero else One in
    let b = if ((value lsr 6) mod 2) = 0 then Zero else One in
    let c = if ((value lsr 5) mod 2) = 0 then Zero else One in
    let d = if ((value lsr 4) mod 2) = 0 then Zero else One in
    let e = if ((value lsr 3) mod 2) = 0 then Zero else One in
    let f = if ((value lsr 2) mod 2) = 0 then Zero else One in
    let g = if ((value lsr 1) mod 2) = 0 then Zero else One in
    let h = if ((value lsr 0) mod 2) = 0 then Zero else One in
    [a;b;c;d;e;f;g;h] in
  let rec helper l =
    match l with
    | h::t -> (char_to_bitlist h) :: (helper t)
    | [] -> [] in
  let bits = helper cl in
  let rec take_n n l =
    if n <= 0 then []
    else match l with h::t -> h :: (take_n (n-1) t) | [] -> [] in
  List.flatten (take_n length bits)


let save_file str fn=
     let channel = open_out fn in
     output_string channel str;
     close_out channel;;

let decompress_string str : string =
  let (length,tree,str) = read_string str in
  let bitlist = string_to_bitlist length str in
  decode_bl bitlist tree

let compress_string str : string =
  do_encode str

let encode_file_name fn nfn=
  let s = read_whole_file fn in
  let sl = float_of_int (String.length s) in
  Printf.printf "Compressing %f bytes\n" (sl);
  let comped = compress_string s in
  let cl = float_of_int (String.length comped) in
  Printf.printf "Compressed to %f bytes\n" (cl);
  Printf.printf "Deflated %f\n" ((cl *. 100.) /.sl);
  save_file comped nfn

let decode_file_name fn nfn =
  let s = read_whole_file fn in
  save_file (decompress_string s) nfn


