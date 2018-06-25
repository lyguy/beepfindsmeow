open Base

module Rand_util = struct
  (** Helper to grab a random element from an array *)
  let array_rand a =
    let i = Array.length a |> Random.int in
    a.(i)

  let n_rand_unique comparater generator size =
    let rec loop s =
      if Int.equal (Set.length s)  size  then Set.to_list s
      else Set.add s (generator ()) |> loop
    in
    if size <= 0 then []
    else loop (Set.empty comparater)
end


module Msg = struct
  type t = string

  let all = [|
    "\"I pity the fool who mistakes me for kitten!\", sez Mr. T.";
    "That's just an old tin can.";
    "It's an altar to the horse god.";
    "A box of dancing mechanical pencils. They dance! They sing!";
    "It's an old Duke Ellington record.";
    "A box of fumigation pellets.";
    "A digital clock. It's stuck at 2:17 PM.";
    "That's just a charred human corpse.";
    "I don't know what that is, but it's not kitten.";
    "An empty shopping bag. Paper or plastic?";
    "Could it be... a big ugly bowling trophy?";
    "A coat hanger hovers in thin air. Odd.";
    "Not kitten, just a packet of Kool-Aid(tm).";
    "A freshly-baked pumpkin pie.";
    "A lone, forgotten comma, sits here, sobbing.";
    "ONE HUNDRED THOUSAND CARPET FIBRES!!!!!";
    "It's Richard Nixon's nose!";
    "It's Lucy Ricardo. \"Aaaah, Ricky!\", she says.";
    "You stumble upon Bill Gates' stand-up act.";
    "Just an autographed copy of the Kama Sutra.";
    "It's the Will Rogers Highway. Who was Will Rogers, anyway?";
    "It's another robot, more advanced in design than you but strangely immobile.";
    "Leonard Richardson is here, asking people to lick him.";
    "It's a stupid mask, fashioned after a beagle.";
    "Your State Farm Insurance(tm) representative!";
    "It's the local draft board.";
    "Seven 1/4\" screws and a piece of plastic.";
    "An 80286 machine.";
    "One of those stupid \"Homes of the Stars\" maps.";
    "A signpost saying \"TO KITTEN\". It points in no particular direction.";
    "A hammock stretched between a tree and a volleyball pole.";
    "A Texas Instruments of Destruction calculator.";
    "It's a dark, amphorous blob of matter.";
    "Just a pincushion.";
    "It's a mighty zombie talking about some love and prosperity.";
    "\"Dear robot, you may have already won our 10 MILLION DOLLAR prize...\"";
    "It's just an object.";
    "A mere collection of pixels.";
    "A badly dented high-hat cymbal lies on its side here.";
    "A marijuana brownie.";
    "A plush Chewbacca.";
    "Daily hunger conditioner from Australasia";
    "Just some stuff.";
    "Why are you touching this when you should be finding kitten?";
    "A glorious fan of peacock feathers.";
    "It's some compromising photos of Babar the Elephant.";
    "A copy of the Weekly World News. Watch out for the chambered nautilus!";
    "It's the proverbial wet blanket.";
    "A \"Get Out of Jail Free\" card.";
    "An incredibly expensive \"Mad About You\" collector plate.";
    "Paul Moyer's necktie.";
    "A haircut and a real job. Now you know where to get one!";
    "An automated robot-hater. It frowns disapprovingly at you.";
    "An automated robot-liker. It smiles at you.";
    "It's a black hole. Don't fall in!";
    "Just a big brick wall.";
    "You found kitten! No, just kidding.";
    "Heart of Darkness brand pistachio nuts.";
    "A smoking branding iron shaped like a 24-pin connector.";
    "It's a Java applet.";
    "An abandoned used-car lot.";
    "A shameless plug for Herd of Nerds: http://www.herdofnerds.net/.";
    "Sherlock Holmes' pipe, upside down.";
    "A can of Spam Lite.";
    "This is another fine mess you've gotten us into, Stanley.";
    "It's scenery for \"Waiting for Godot\".";
    "This grain elevator towers high above you.";
    "A Mentos wrapper.";
    "It's the constellation Pisces.";
    "It's a fly on the wall. Hi, fly!";
    "This kind of looks like kitten, but it's not.";
    "It's a banana! Oh, joy!";
    "A helicopter has crashed here.";
    "Carlos Tarango stands here, doing his best impression of Pat Smear.";
    "A patch of mushrooms grows here.";
    "A patch of grape jelly grows here.";
    "A spindle, and a grindle, and a bucka-wacka-woom!";
    "A geyser sprays water high into the air.";
    "A toenail? What good is a toenail?";
    "You've found the fish! Not that it does you much good in this game.";
    "A Buttertonsils bar.";
    "One of the few remaining discoes.";
    "Ah, the uniform of a Revolutionary-era minuteman.";
    "A punch bowl, filled with punch and lemon slices.";
    "It's nothing but a G-thang, baby.";
    "IT'S ALIVE! AH HA HA HA HA!";
    "This was no boating accident!";
    "Wait! This isn't the poker chip! You've been tricked! DAMN YOU, MENDEZ!";
    "A livery stable! Get your livery!";
    "It's a perpetual immobility machine.";
    "\"On this spot in 1962, Henry Winkler was sick.\"";
    "There's nothing here; it's just an optical illusion.";
    "The World's Biggest Motzah Ball!";
    "A tribe of cannibals lives here. They eat Malt-O-Meal for breakfast, you know.";
    "This appears to be a rather large stack of trashy romance novels.";
    "Look out! Exclamation points!";
    "A herd of wild coffee mugs slumbers here.";
    "It's a limbo bar! How low can you go?";
    "It's the horizon. Now THAT'S weird.";
    "A vase full of artificial flowers is stuck to the floor here.";
    "A large snake bars your way.";
    "A pair of saloon-style doors swing slowly back and forth here.";
    "It's an ordinary bust of Beethoven... but why is it painted green?";
    "It's TV's lovable wisecracking Crow! \"Bite me!\", he says.";
    "Hey, look, it's war. What is it good for? Absolutely nothing. Say it again.";
    "It's the amazing self-referential thing that's not kitten.";
    "A flamboyant feather boa. Now you can dress up like Carol Channing!";
    "\"Sure hope we get some rain soon,\" says Farmer Joe.";
    "\"How in heck can I wash my neck if it ain't gonna rain no more?\" asks Farmer Al.";
    "\"Topsoil's all gone, ma,\" weeps Lil' Greg.";
    "This is a large brown bear. Oddly enough, it's currently peeing in the woods.";
    "A team of arctic explorers is camped here.";
    "This object here appears to be Louis Farrakhan's bow tie.";
    "This is the world-famous Chain of Jockstraps.";
    "A trash compactor, compacting away.";
    "This toaster strudel is riddled with bullet holes!";
    "It's a hologram of a crashed helicopter.";
    "This is a television. On screen you see a robot strangely similar to yourself.";
    "This balogna has a first name, it's R-A-N-C-I-D.";
    "A salmon hatchery? Look again. It's merely a single salmon.";
    "It's a rim shot. Ba-da-boom!";
    "It's creepy and it's kooky, mysterious and spooky. It's also somewhat ooky.";
    "This is an anagram.";
    "This object is like an analogy.";
    "It's a symbol. You see in it a model for all symbols everywhere.";
    "The object pushes back at you.";
    "A traffic signal. It appears to have been recently vandalized.";
    "\"There is no kitten!\" cackles the old crone. You are shocked by her blasphemy.";
    "This is a Lagrange point. Don't come too close now.";
    "The dirty old tramp bemoans the loss of his harmonica.";
    "Look, it's Fanny the Irishman!";
    "What in blazes is this?";
    "It's the instruction manual for a previous version of this game.";
    "A brain cell. Oddly enough, it seems to be functioning.";
    "Tea and/or crumpets.";
    "This jukebox has nothing but Cliff Richards albums in it.";
    "It's a Quaker Oatmeal tube, converted into a drum.";
    "This is a remote control. Being a robot, you keep a wide berth.";
    "It's a roll of industrial-strength copper wire.";
    "Oh boy! Grub! Er, grubs.";
    "A puddle of mud, where the mudskippers play.";
    "Plenty of nothing.";
    "Look at that, it's the Crudmobile.";
    "Just Walter Mattheau and Jack Lemmon.";
    "Two crepes, two crepes in a box.";
    "An autographed copy of \"Primary Colors\", by Anonymous.";
    "Another rabbit? That's three today!";
    "It's a segmentation fault. Core dumped, by the way.";
    "A historical marker showing the actual location of /dev/null.";
    "Thar's Mobius Dick, the convoluted whale. Arrr!";
    "It's a charcoal briquette, smoking away.";
    "A pizza, melting in the sun.";
    "It's a \"HOME ALONE 2: Lost in New York\" novelty cup.";
    "A stack of 7 inch floppies wobbles precariously.";
    "It's nothing but a corrupted floppy. Coaster anyone?";
    "A section of glowing phosphor cells sings a song of radiation to you.";
    "This TRS-80 III is eerily silent.";
    "A toilet bowl occupies this space.";
    "This peg-leg is stuck in a knothole!";
    "It's a solitary vacuum tube.";
    "This corroded robot is clutching a mitten.";
    "\"Hi, I'm Anson Williams, TV's 'Potsy'.\"";
    "This subwoofer was blown out in 1974.";
    "Three half-pennies and a wooden nickel.";
    "It's the missing chapter to \"A Clockwork Orange\".";
    "It's a burrito stand flyer. \"Taqueria El Ranchito\".";
    "This smiling family is happy because they eat LARD.";
    "Roger Avery, persona un famoso de los Estados Unidos.";
    "Ne'er but a potted plant.";
    "A parrot, kipping on its back.";
    "A forgotten telephone switchboard.";
    "A forgotten telephone switchboard operator.";
    "It's an automated robot-disdainer. It pretends you're not there.";
    "It's a portable hole. A sign reads: \"Closed for the winter\".";
    "Just a moldy loaf of bread.";
    "A little glass tub of Carmex. ($.89) Too bad you have no lips.";
    "A Swiss-Army knife. All of its appendages are out. (toothpick lost)";
    "It's a zen simulation, trapped within an ASCII character.";
    "It's a copy of \"The Rubaiyat of Spike Schudy\".";
    "It's \"War and Peace\" (unabridged, very small print).";
    "A willing, ripe tomato bemoans your inability to digest fruit.";
    "A robot comedian. You feel amused.";
    "It's KITT, the talking car.";
    "Here's Pete Peterson. His batteries seem to have long gone dead.";
    "\"Blup, blup, blup\", says the mud pot.";
    "More grist for the mill.";
    "Grind 'em up, spit 'em out, they're twigs.";
    "The boom box cranks out an old Ethel Merman tune.";
    "It's \"Finding kitten\", published by O'Reilly and Associates.";
    "Pumpkin pie spice.";
    "It's the Bass-Matic '76! Mmm, that's good bass!";
    "\"Lend us a fiver 'til Thursday\", please Andy Capp.";
    "It's a tape of '70s rock. All original hits! All original artists!";
    "You've found the fabled America Online disk graveyard!";
    "Empty jewelboxes litter the landscape.";
    "It's the astounding meta-object.";
    "Ed McMahon stands here, lost in thought. Seeing you, he bellows, \"YES SIR!\"";
    "...thingy???";
    "It's 1000 secrets the government doesn't want you to know!";
    "The letters O and R.";
    "A magical... magic thing.";
  |]

  let rand () = Rand_util.array_rand all
end


module Sigal = struct
  type t = char
  let all = [|
    '0';
    '1';
    '2';
    '3';
    '4';
    '5';
    '6';
    '7';
    '8';
    '9';
    ',';
    'a';
    'b';
    'c';
    'd';
    'e';
    'f';
    'g';
    'h';
    'i';
    'j';
    'k';
    'l';
    'm';
    'n';
    'o';
    'p';
    'q';
    'r';
    's';
    't';
    'u';
    'v';
    'w';
    'x';
    'y';
    'z';
    'A';
    'B';
    'C';
    'D';
    'E';
    'F';
    'G';
    'H';
    'I';
    'J';
    'K';
    'L';
    'M';
    'N';
    'O';
    '<';
    'P';
    'Q';
    'R';
    'S';
    'T';
    'U';
    'V';
    'W';
    'X';
    'Y';
    'Z';
    '!';
    '$';
    '|';
    '%';
    '^';
    '&';
    '*';
    '(';
    ')';
    '-';
    '+';
    '=';
    '[';
    ']';
    '{';
    '}';
    '~';
    '@';
    '.';
    ':';
    '?';
    '/';
    '>';
  |]

  let rand () = Rand_util.array_rand all
end

module Color = struct
  type t =
    | White
    | Red
    | Green
    | Blue
  [@@deriving enumerate]
  let all = all |> Array.of_list
  let rand () = Rand_util.array_rand all
end

module Avatar = struct
  type t = Sigal.t * Color.t
  let rand () = Sigal.rand () , Color.rand ()
end

module Item = struct
  type t =
    | Robot
    | Kitten of Avatar.t
    | Obstacle of Avatar.t * Msg.t

  let rand_obs () = Obstacle (Avatar.rand () , Msg.rand ())

  let rand_kitten () = Kitten (Avatar.rand ())
end


module Coord = struct
  type t = (int * int)
  let compare = [%compare: (int * int)]
  let sexp_of_t = [%sexp_of: (int * int)]
  include (val Comparator.make ~compare ~sexp_of_t)

  let rand w h = (Random.int w, Random.int h)
end

module Xy_board : sig
  type 'a t

  val of_alist_or_error: w:int ->  h:int -> (Coord.t * 'a) list -> ('a t) Or_error.t

  val size: 'a t -> int * int

  val get: ('a t) -> Coord.t -> ('a option) Or_error.t

  val set: ('a t) -> Coord.t -> 'a -> ('a t) Or_error.t

  val remove: ('a t) -> Coord.t -> ('a t) Or_error.t
end = struct

  type 'a t = {
    (* width * height *)
    size: int * int;
    contents: (Coord.t, 'a , Coord.comparator_witness) Map.t;
  }

  let valid_bounds w h = 0 < h && 0 < w

  let valid_coord  ~size ~coord =
    let (w, h) = size in
    let (x, y) = coord in
    0 <= x && x < w &&
    0 <= y && y < h

  let valid_coord_or_error ~size ~coord =
    let (w, h) = size in
    let (x, y) = coord in
    if valid_coord ~size ~coord
    then Ok coord
    else Or_error.errorf "Coordinates out of bounds: x,y = (%i, %i) but w,h = (%i, %i)" x y w h

  let size t = t.size

  let get t coord =
    let open Or_error.Let_syntax in
    let%map i = valid_coord_or_error ~size:t.size ~coord in
    Map.find t.contents i

  let set t coord v =
    let open Or_error.Let_syntax in
    let%map i = valid_coord_or_error ~size:t.size ~coord in
    { t with contents = Map.set t.contents ~key:i ~data:v }

  let remove t coord =
    let open Or_error.Let_syntax in
    let%map c = valid_coord_or_error ~size:t.size ~coord in
    { t with contents = Map.remove t.contents c } 

  let of_alist_or_error ~w ~h alist =
    let size = (w, h) in

    let item_ok (coord, item) =
      let open Or_error.Let_syntax in
      let%map i = valid_coord_or_error ~size ~coord in
      (i, item)
    in

    let build_contents items =
      let open Or_error.Let_syntax in
      let%bind items_indexed = List.map items ~f:item_ok
                               |> Or_error.combine_errors in
      let%bind contents =
        Map.of_alist_or_error (module Coord) items_indexed in
      Ok {
        size;
        contents;
      }
    in
    match valid_bounds h w with
    | false -> Or_error.errorf "Height and Width must be positive: h:%i w%i" h w
    | true -> build_contents alist
end

module Board : sig
  type t

  type dir = Up | Down | Left | Right

  val create_rand : height:int -> width:int -> n_items:int -> t Or_error.t

  (** gives the size of the board in hight * width*)
  val size : t -> int * int

  val get : t -> int * int -> Item.t option

  val move: t -> dir -> t

end = struct

  type t = {
    board: Item.t Xy_board.t;
    robot_pos: Coord.t;
    kitten_pos: Coord.t;
  }

  type dir = Up | Down | Left | Right

  let create_rand ~height ~width ~n_items =
    let frac = 5 in

    if height <= 0 || width <= 0 || n_items <= 0
    then Or_error.errorf "Height, width, and n_items must be positive: height: %i, width: %i, n_items: %i" height width n_items
    else if (height * width) <= n_items * frac
    then Or_error.errorf "n_items must be less than 1/%i of the total squares (height * width)" frac
    else
      match Rand_util.n_rand_unique (module Coord) (fun () -> Coord.rand width height) (n_items + 2) with
      | [] | _ :: []  -> Or_error.error_string "n_rand_unique didn't return enough items. This is a bug."
      | robot_pos :: kitten_pos :: items -> begin
          let open Or_error.Let_syntax in
          let%map board =
            (robot_pos, Item.Robot) :: (kitten_pos, Item.rand_kitten ()) :: List.map items ~f:(fun c -> (c, Item.rand_obs ()))
            |> Xy_board.of_alist_or_error ~h:height ~w:width in
          {
            board;
            robot_pos;
            kitten_pos;
          }
        end

  let get t xy = match Xy_board.get t.board xy with
    | Ok i -> i
    | Error _ -> None

  let size t = Xy_board.size t.board

  let move t dir =
    let open Or_error.Let_syntax in

    let target =
      let (x,y) = t.robot_pos in
      match dir with
      | Up -> (x, y - 1)
      | Down -> (x, y + 1)
      | Left -> (x - 1, y)
      | Right -> (x + 1, y)
    in

    let move_robot t =
      begin
        let%bind b1 = Xy_board.set t.board target Item.Robot in
        let%map b2 = Xy_board.remove b1 t.robot_pos in
        {
          board = b2;
          robot_pos = target;
          kitten_pos = t.kitten_pos;
        }
      end
      |> Or_error.ok
      |> Option.value ~default:t
    in

    match Xy_board.get t.board target with
    | Error _ -> t
    | Ok contents -> match contents with
      | Some _ -> t
      | None -> (move_robot t)
end
