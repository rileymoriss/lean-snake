/-
  Pure game logic for Snake. Nothing in this file touches the screen,
  the keyboard or the clock: a `Game` value goes in, a `Game` value comes out.
-/
namespace Snake

inductive Dir where
  | up | down | left | right
  deriving BEq, Repr, Inhabited

namespace Dir

def opposite : Dir → Dir
  | up => down
  | down => up
  | left => right
  | right => left

def dx : Dir → Int
  | left => -1
  | right => 1
  | _ => 0

def dy : Dir → Int
  | up => -1
  | down => 1
  | _ => 0

end Dir

/-- A cell on the board. `(0, 0)` is the top-left cell. -/
structure Pos where
  x : Int
  y : Int
  deriving BEq, Repr, Inhabited

inductive Phase where
  | ready    -- waiting for the first key press
  | playing
  | paused
  | over     -- hit a wall or itself
  | won      -- the snake fills the whole board
  deriving BEq, Repr, Inhabited

structure Game where
  cols : Nat
  rows : Nat
  /-- Snake cells, head first. -/
  body : List Pos
  dir : Dir
  /-- Turns typed ahead by the player, applied one per step. -/
  pending : List Dir
  food : Pos
  score : Nat
  best : Nat
  phase : Phase
  deriving Inhabited

/-- Every cell that is not covered by the snake. -/
def freeCells (cols rows : Nat) (body : List Pos) : Array Pos := Id.run do
  let mut out : Array Pos := #[]
  for y in [0:rows] do
    for x in [0:cols] do
      let p : Pos := ⟨Int.ofNat x, Int.ofNat y⟩
      if !body.contains p then
        out := out.push p
  return out

/-- Pick a free cell for the food, or `none` when the board is full. -/
def placeFood (cols rows : Nat) (body : List Pos) (rnd : Nat) : Option Pos :=
  let free := freeCells cols rows body
  if free.size == 0 then none else free[rnd % free.size]?

/-- A fresh game: a three-cell snake in the middle of the board, facing right. -/
def Game.new (cols rows best rnd : Nat) : Game :=
  let cx : Int := Int.ofNat (cols / 2)
  let cy : Int := Int.ofNat (rows / 2)
  let body : List Pos := [⟨cx, cy⟩, ⟨cx - 1, cy⟩, ⟨cx - 2, cy⟩]
  { cols, rows, body
    dir := .right
    pending := []
    food := (placeFood cols rows body rnd).getD ⟨0, 0⟩
    score := 0
    best
    phase := .ready }

/-- Queue a turn. Reversing onto yourself and repeating the current
    direction are ignored; at most three turns are buffered. -/
def Game.turn (g : Game) (d : Dir) : Game :=
  let last := g.pending.getLast?.getD g.dir
  if d == last || d == last.opposite || g.pending.length ≥ 3 then g
  else { g with pending := g.pending ++ [d] }

/-- Advance the snake by one cell. -/
def Game.step (g : Game) (rnd : Nat) : Game :=
  if g.phase != .playing then g else
  let (dir, pending) :=
    match g.pending with
    | d :: rest => (d, rest)
    | [] => (g.dir, [])
  match g.body with
  | [] => { g with phase := .over }
  | head :: _ =>
    let nh : Pos := ⟨head.x + dir.dx, head.y + dir.dy⟩
    let eats := nh == g.food
    -- The tail cell moves away this step unless the snake grows,
    -- so the head is allowed to move into it.
    let rest := if eats then g.body else g.body.dropLast
    let outside :=
      decide (nh.x < 0) || decide (nh.y < 0) ||
      decide (nh.x ≥ Int.ofNat g.cols) || decide (nh.y ≥ Int.ofNat g.rows)
    if outside || rest.contains nh then
      { g with dir, pending := [], phase := .over }
    else
      let body := nh :: rest
      if eats then
        let score := g.score + 1
        let best := max g.best score
        match placeFood g.cols g.rows body rnd with
        | some food => { g with body, dir, pending, food, score, best }
        | none => { g with body, dir, pending := [], score, best, phase := .won }
      else
        { g with body, dir, pending }

/-- Milliseconds between steps. The snake speeds up as it grows. -/
def Game.interval (g : Game) : Nat :=
  150 - min 80 (g.score * 3)

end Snake
