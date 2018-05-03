(* Test wrong type all by itself *)
module type Sig = sig
  val hasWrongType: string -> unit
end

module MyModule : Sig = struct
  let hasWrongType s = 0
end

