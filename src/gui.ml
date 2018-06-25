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
    let%map game = Game.create_rand ~height:60 ~width:100 ~n_items:20 in
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


module Menu_ui : sig
  type t =
    | New_game
    | Quit
  val empty : t
  val binding: t ->  [ `Down | `Left | `Right | `Up ] -> t
  val draw: t -> Notty.I.t
end = struct
  type t =
    | New_game
    | Quit

  let next t =
    match t with
    | New_game -> Quit
    | Quit -> Quit

  let prev t =
    match t with
    | New_game -> New_game
    | Quit -> New_game


  let binding t e = match e with
    | `Down -> next t
    | `Up -> prev t
    | `Left | `Right -> t

  let empty = New_game


  let imgafy ss a = List.map ss ~f:(fun i -> Notty.I.string a i)
                    |> Notty.I.vcat

  let beep =imgafy
      [" , __              ";
       "/|/  \             ";
       " | __/ _   _    _  ";
       " |   \\|/  |/  |/ \\_";
       " |(__/|__/|__/|__/ ";
       "             /|    ";
       "             \\|    ";]


  let finds = imgafy
      [" _                       ";
       "| | o             |      ";
       "| |     _  _    __|   ,  ";
       "|/  |  / |/ |  /  |  / \\_";
       "|__/|_/  |  |_/\\_/|_/ \\/ ";
       "|\                       ";
       "|/                       ";]


  let meow = imgafy
      [" ,__ __                    ";
       "/|  |  |                   ";
       " |  |  |   _   __          ";
       " |  |  |  |/  /  \\_|  |  |_";
       " |  |  |_/|__/\\__/  \\/ \\/  ";
       "                           ";
       "                           ";]

  let banner = I.(
      beep(A.st A.bold) <|> finds(A.empty) |> hpad 0 1 <|> meow(A.st A.bold)
    )


  let draw t =
    let menu = match t with
      | New_game ->  I.(string A.(bg white ++ fg black) "New Game"  <-> string A.empty "Quit") 
      | Quit -> I.(string A.empty "New Game"  <-> string A.(bg white ++ fg black) "Quit")
    in
    I.(banner <-> menu)
end


module Ui : sig
  type t

  val create: unit -> t

  val draw : t -> I.t

  val is_end : t -> bool

  val handle_event: t -> Unescape.event -> t
end = struct

  type t =
    | Menu of Menu_ui.t
    | Game of Game_ui.t
    | End

  let create () = Menu (Menu_ui.empty)

  let draw t = match t with
    | Menu state -> Menu_ui.draw state
    | Game state -> Game_ui.draw state
    | End -> I.empty

  let is_end t = match t with
    | End -> true
    | Game _ | Menu _ -> false


  let game_binding state event =
    match event with
    | `Key (`Escape, _)     -> Menu (Menu_ui.empty)
    | `Key (`Arrow arrow,_) -> Game (Game_ui.binding state arrow)
    | _                     -> Game state

  let menu_binding state event =
    let new_game () =
      match Game_ui.create () with
      | Error _ -> Menu (Menu_ui.empty)
      | Ok b ->  Game b
    in
    match event with
    | `Key (`Arrow arrow ,_) -> Menu (Menu_ui.binding state arrow)
    | `Key (`Enter,_) ->
      begin
        match state with
        | Menu_ui.Quit -> End
        | Menu_ui.New_game -> new_game ()

      end
    | _ -> Menu (Menu_ui.empty)

  let handle_event t e =
    match t with
    | Menu state -> menu_binding state e
    | Game state -> game_binding state e
    | End -> End
end


module App : sig
  val main: unit -> unit
end = struct
  open Notty_unix

  let main_loop term =
    let rec loop t ui =
      if Ui.is_end ui then () else begin
        Ui.draw ui |> Term.image t;
        match Term.event t with
        | `End | `Key (`ASCII 'C', [`Ctrl]) | `Key (`ASCII 'D', [`Ctrl]) -> ()
        | `Resize _ -> loop t ui
        | #Unescape.event as e -> Ui.handle_event ui e |> loop t
      end in
    loop term (Ui.create ())


  let main () =
    Base.Random.self_init ();
    let t = Term.create () in
    main_loop t;
    Term.release t
end
