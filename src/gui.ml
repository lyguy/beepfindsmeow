open Base
open Notty

module Game_ui : sig
  type t
  val create : unit -> t Or_error.t
  val binding : t -> [ `Down | `Left | `Right | `Up ] -> t
  val draw: t -> Notty.I.t
end = struct

  type t = Game.t * string option

  let create () =
    let open Base.Or_error.Let_syntax in
    let%map game = Game.create_rand ~height:20 ~width:20 ~n_items:20 in
    (game, None)

  let binding (b, msg) arrow =
    let dir =
      match arrow with
      | `Left  -> Game.Left
      | `Right -> Game.Right
      | `Up    -> Game.Up
      | `Down  -> Game.Down in
    let (b_new, m_opt) = Game.move b dir in
    match m_opt with
    | None -> (b_new, msg)
    | Some _ as msg_new-> (b_new, msg_new)
    
  let draw (b, m) =
    let avatar (s, c) =
      let a = match c with
        | Game.Color.White -> A.white
        | Game.Color.Red -> A.red
        | Game.Color.Green -> A.green
        | Game.Color.Blue -> A.blue
      in
      I.char A.(fg a) s 1 1
    in

    let item i =
      match i with
      | Game.Item.Robot -> I.char A.empty '#' 1 1
      | Game.Item.Kitten a | Game.Item.Obstacle (a,_) -> avatar a
    in

    let msg w m =
      let msg_i =
        match m with
        | None -> I.void 1 1
        | Some m -> I.string A.empty m in
      I.(msg_i <-> char A.empty '-' w 1)
    in

    let (w, h) = Game.size b in
    let f x y = match Game.get b (x,y) with
      | None -> I.void 1 1
      | Some i -> item i
    in
    I.(
      msg w m
      <->
      I.tabulate w h f
    )
end


module Ui : sig
  type t

  val create: unit -> t

  val draw : t -> I.t

  val handle_event: t -> Unescape.event -> t
end = struct

  type t =
    | Menu
    | Game of Game_ui.t

  let create () = Menu

  let draw t = match t with
    | Menu -> I.empty
    | Game state -> Game_ui.draw state


  let game_binding state event =
    match event with
    | `Key (`Escape, _)       -> Menu
    | `Key (`Arrow arrow,_)   -> Game (Game_ui.binding state arrow)
    | _                       -> Game state

  let menu_binding event =
    match event with
    | `Key (`Enter,_) ->
      begin
        match Game_ui.create () with
        | Error _ -> Menu (* fix this *)
        | Ok b ->  Game b
      end
    | _ -> Menu

  let handle_event t e =
    match t with
    | Menu -> menu_binding e
    | Game state -> game_binding state e
end


module App : sig
  val main: unit -> unit
end = struct
  open Notty_unix

  let main_loop term =
    let rec loop t ui =
      Ui.draw ui |> Term.image t;
      match Term.event t with
      | `End | `Key (`ASCII 'C', [`Ctrl]) | `Key (`ASCII 'D', [`Ctrl]) -> ()
      | `Resize _ -> loop t ui
      | #Unescape.event as e -> Ui.handle_event ui e |> loop t in
    loop term (Ui.create ())


  let main () =
    Base.Random.self_init (); 
    let t = Term.create () in
    main_loop t;
    Term.release t
end
