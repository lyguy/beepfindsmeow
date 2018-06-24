open Notty
open Game

let avatar (s, c) =
  let a = match c with
    | Color.White -> A.white
    | Color.Red -> A.red
    | Color.Green -> A.green
    | Color.Blue -> A.blue
  in
  I.char A.(fg a) s 1 1

let item i =
  match i with
  | Item.Robot -> I.char A.empty '#' 1 1
  | Item.Kitten a | Item.Obstacle (a,_) -> avatar a


let board b =
  let (h, w) = Board.size b in
  let f x y = match Board.get b (x,y) with
    | None -> I.void 1 1
    | Some i -> item i
  in
  I.tabulate w h f
