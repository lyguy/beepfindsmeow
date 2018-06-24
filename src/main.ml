open Notty_unix

let game_loop t =
  let rec update t b =
    Term.image t (Gui.board b);
    loop t b
  and loop t b =
    match Term.event t with
    | `Key (`Enter,_)         -> ()
    | `Key (`Arrow `Left,_)   -> update t Game.Board.(move b Left)
    | `Key (`Arrow `Right,_)  -> update t Game.Board.(move b Right)
    | `Key (`Arrow `Up,_)     -> update t Game.Board.(move b Up)
    |  `Key (`Arrow `Down,_)  -> update t Game.Board.(move b Down)
    | _                       -> loop t b
  in
  match Game.Board.(create_rand ~height:20 ~width:20 ~n_items:20) with
  | Error _ -> ()
  | Ok b -> update t b


let main () =
  let t = Term.create () in
  game_loop t;
  Term.release t

let () = main ()
