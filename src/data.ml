type t =
  { cardinal : int }
    [@@deriving yojson]

let of_file : string -> t = fun _ -> { cardinal = 0 }
