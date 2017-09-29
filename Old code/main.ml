open Hex

let main() = 
  Arg.parse [] (fun s -> Hex.pp_grid_bool (Hex.from_channel (open_in s))) "Pingouin !!"

let () = main()
