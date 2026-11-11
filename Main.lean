import Snake

open Snake Win32

/-! ## Board size and keys -/

def boardCols : Nat := 24
def boardRows : Nat := 18

def vkEnter : Nat := 0x0D
def vkEscape : Nat := 0x1B
def vkSpace : Nat := 0x20
def vkLeft : Nat := 0x25
def vkUp : Nat := 0x26
def vkRight : Nat := 0x27
def vkDown : Nat := 0x28

/-- Every key the game listens to. Letters use their upper-case ASCII code. -/
def watchedKeys : Array Nat :=
  #[vkLeft, vkUp, vkRight, vkDown, 'W'.toNat, 'A'.toNat, 'S'.toNat, 'D'.toNat,
    vkSpace, vkEnter, vkEscape, 'P'.toNat, 'R'.toNat]

def keyDir (vk : Nat) : Option Dir :=
  if vk == vkLeft || vk == 'A'.toNat then some .left
  else if vk == vkRight || vk == 'D'.toNat then some .right
  else if vk == vkUp || vk == 'W'.toNat then some .up
  else if vk == vkDown || vk == 'S'.toNat then some .down
  else none

/-- React to one key press. -/
def handleKey (g : Game) (vk rnd : Nat) : Game :=
  let fresh := Game.new g.cols g.rows g.best rnd
  match keyDir vk with
  | some d =>
    match g.phase with
    | .ready =>
      -- The snake starts facing right, so "left" just starts it moving.
      { g with dir := if d == g.dir.opposite then g.dir else d, phase := .playing }
    | .playing => g.turn d
    | .paused => { g with phase := .playing }.turn d
    | _ => g
  | none =>
    if vk == 'R'.toNat then fresh
    else if vk == vkSpace || vk == vkEnter then
      match g.phase with
      | .ready => { g with phase := .playing }
      | .playing => { g with phase := .paused }
      | .paused => { g with phase := .playing }
      | _ => fresh
    else if vk == 'P'.toNat || vk == vkEscape then
      match g.phase with
      | .playing => { g with phase := .paused }
      | .paused => { g with phase := .playing }
      | _ => g
    else g

/-! ## Drawing -/

/-- Everything the renderer needs: the window, two off-screen surfaces
    (`back` is composed each frame, `board` holds the static background),
    pixel sizes, brushes and fonts. -/
structure Gfx where
  hwnd : UInt64
  back : UInt64
  board : UInt64
  width : Nat
  height : Nat
  cell : Nat
  hud : Nat
  /-- Scale a size designed at 96 DPI to the real display DPI. -/
  dpi : Nat
  snakeBrush : UInt64
  headBrush : UInt64
  deadBrush : UInt64
  eyeBrush : UInt64
  foodBrush : UInt64
  shineBrush : UInt64
  panelBrush : UInt64
  borderBrush : UInt64
  bigFont : UInt64
  hudFont : UInt64
  smallFont : UInt64

def Gfx.sc (x : Gfx) (n : Nat) : Nat := (n * x.dpi + 48) / 96

def textColor : UInt64 := rgb 0xE6 0xED 0xF3
def dimColor : UInt64 := rgb 0x8B 0x94 0x9E
def accentColor : UInt64 := rgb 0x56 0xD3 0x64

/-- A centred message box over the board. -/
def drawPanel (x : Gfx) (title subtitle : String) : IO Unit := do
  let pw := x.sc 470
  let ph := x.sc 116
  let px := (x.width - pw) / 2
  let py := x.hud + (x.height - x.hud - ph) / 2
  let b := max 1 (x.sc 2)
  fillRoundRect x.back x.borderBrush px py pw ph (x.sc 18)
  fillRoundRect x.back x.panelBrush (px + b) (py + b) (pw - 2 * b) (ph - 2 * b) (x.sc 16)
  drawText x.back x.bigFont textColor .center (x.width / 2) (py + x.sc 20) title
  drawText x.back x.smallFont dimColor .center (x.width / 2) (py + x.sc 72) subtitle

/-- Compose one frame off-screen and copy it to the window. -/
def render (x : Gfx) (g : Game) : IO Unit := do
  let c := x.cell
  let pad := max 1 (c / 10)
  let inner := c - 2 * pad
  let corner := c * 2 / 5
  let px (p : Pos) : Nat := p.x.toNat * c
  let py (p : Pos) : Nat := x.hud + p.y.toNat * c
  blit x.back x.board x.width x.height

  -- Food.
  if g.phase != .won then
    let f := g.food
    let fp := pad + max 1 (c / 14)
    fillEllipse x.back x.foodBrush (px f + fp) (py f + fp) (c - 2 * fp) (c - 2 * fp)
    fillEllipse x.back x.shineBrush (px f + c * 3 / 10) (py f + c * 3 / 10) (c / 6) (c / 6)

  -- Snake: bridges between neighbouring cells first, then the cells themselves.
  let mut prev : Option Pos := none
  for seg in g.body do
    if let some a := prev then
      if a.y == seg.y then
        let l := if a.x < seg.x then a else seg
        fillRect x.back x.snakeBrush (px l + c - pad - corner) (py l + pad) (2 * pad + 2 * corner) inner
      else
        let t := if a.y < seg.y then a else seg
        fillRect x.back x.snakeBrush (px t + pad) (py t + c - pad - corner) inner (2 * pad + 2 * corner)
    prev := some seg
  for seg in g.body.drop 1 do
    fillRoundRect x.back x.snakeBrush (px seg + pad) (py seg + pad) inner inner corner
  if let some h := g.body.head? then
    let brush := if g.phase == .over then x.deadBrush else x.headBrush
    fillRoundRect x.back brush (px h + pad) (py h + pad) inner inner corner
    -- Eyes sit toward the direction of travel.
    let e := max 3 (c / 6)
    let fwd := c / 6
    let side := c / 5
    let cx := px h + c / 2
    let cy := py h + c / 2
    let (ax, ay, bx, by') :=
      match g.dir with
      | .right => (cx + fwd, cy - side, cx + fwd, cy + side)
      | .left => (cx - fwd, cy - side, cx - fwd, cy + side)
      | .up => (cx - side, cy - fwd, cx + side, cy - fwd)
      | .down => (cx - side, cy + fwd, cx + side, cy + fwd)
    fillEllipse x.back x.eyeBrush (ax - e / 2) (ay - e / 2) e e
    fillEllipse x.back x.eyeBrush (bx - e / 2) (by' - e / 2) e e

  -- Score bar.
  let ty := (x.hud - x.sc 24) / 2
  drawText x.back x.hudFont textColor .left (x.sc 18) ty s!"Score  {g.score}"
  drawText x.back x.hudFont accentColor .center (x.width / 2) ty "LEAN SNAKE"
  drawText x.back x.hudFont dimColor .right (x.width - x.sc 18) ty s!"Best  {g.best}"

  -- Messages.
  match g.phase with
  | .ready => drawPanel x "Ready?" "Press an arrow key or W A S D to start"
  | .paused => drawPanel x "Paused" "Press Space to continue"
  | .over => drawPanel x "Game Over" s!"You scored {g.score}  -  press Space to play again"
  | .won => drawPanel x "You Win!" s!"Perfect score of {g.score}  -  press Space to play again"
  | .playing => pure ()

  let dc ← getDC x.hwnd
  blit dc x.back x.width x.height
  releaseDC x.hwnd dc

/-! ## Window setup -/

/-- Create the game window with a client area of exactly `w × h` pixels. -/
def createGameWindow (w h : Nat) : IO UInt64 := do
  let inst ← getModuleHandle 0
  -- The window needs no custom message handling, so Windows' own default
  -- window procedure serves as the window procedure. Look up its address.
  let user32 ← getModuleHandle (← pinStr "user32.dll")
  let defProc ← getProcAddress user32 (← pinStr "DefWindowProcA")
  if defProc == 0 then throw (IO.userError "DefWindowProcA not found")
  let className ← pinStr "LeanSnakeWindow"
  let cursor ← loadCursor 0 32512 -- IDC_ARROW
  -- WNDCLASSA, 72 bytes on 64-bit Windows.
  let wc := ByteArray.empty
  let wc := pushU32 wc 0          -- style
  let wc := pushU32 wc 0          -- (padding)
  let wc := pushU64 wc defProc    -- lpfnWndProc
  let wc := pushU32 wc 0          -- cbClsExtra
  let wc := pushU32 wc 0          -- cbWndExtra
  let wc := pushU64 wc inst       -- hInstance
  let wc := pushU64 wc 0          -- hIcon
  let wc := pushU64 wc cursor     -- hCursor
  let wc := pushU64 wc 0          -- hbrBackground (the game paints everything)
  let wc := pushU64 wc 0          -- lpszMenuName
  let wc := pushU64 wc className  -- lpszClassName
  if (← registerClass (← pin wc)) == 0 then
    throw (IO.userError "RegisterClassA failed")
  -- WS_CAPTION | WS_SYSMENU | WS_MINIMIZEBOX: a fixed-size window.
  let style : UInt64 := 0x00CA0000
  -- Ask Windows how big the whole window must be for this client area.
  -- It rewrites the RECT in place.
  let rect := pushU32 (pushU32 (pushU32 (pushU32 ByteArray.empty 0) 0) (u w)) (u h)
  adjustWindowRectEx (← pin rect) style
  let outerW := (readI32 rect 8 - readI32 rect 0).toNat
  let outerH := (readI32 rect 12 - readI32 rect 4).toNat
  let screenW ← getSystemMetrics 0
  let screenH ← getSystemMetrics 1
  let hwnd ← createWindowEx className (← pinStr "Snake - written in Lean 4") style
    ((screenW - outerW) / 2) ((screenH - outerH) / 2) outerW outerH inst
  if hwnd == 0 then throw (IO.userError "CreateWindowExA failed")
  return hwnd

/-- An off-screen drawing surface compatible with the window. -/
def createSurface (windowDC : UInt64) (w h : Nat) : IO UInt64 := do
  let dc ← createCompatibleDC windowDC
  selectObject dc (← createCompatibleBitmap windowDC w h)
  selectObject dc (← getStockObject 8) -- NULL_PEN: shapes are filled without outlines
  setTransparentText dc
  return dc

def setup : IO Gfx := do
  setProcessDPIAware
  let screen ← getDC 0
  let dpi := max 96 (← getDeviceCaps screen 88) -- LOGPIXELSX
  releaseDC 0 screen
  let sc (n : Nat) : Nat := (n * dpi + 48) / 96
  let cell := sc 28
  let hud := sc 52
  let width := boardCols * cell
  let height := hud + boardRows * cell
  let hwnd ← createGameWindow width height
  let windowDC ← getDC hwnd
  let back ← createSurface windowDC width height
  let board ← createSurface windowDC width height
  releaseDC hwnd windowDC

  -- Paint the static background once: score bar plus a checkered board.
  fillRect board (← createSolidBrush (rgb 0x0D 0x11 0x17)) 0 0 width hud
  fillRect board (← createSolidBrush (rgb 0x30 0x36 0x3D)) 0 (hud - max 1 (sc 1)) width (max 1 (sc 1))
  let dark ← createSolidBrush (rgb 0x16 0x1B 0x22)
  let light ← createSolidBrush (rgb 0x1B 0x21 0x29)
  for row in [0:boardRows] do
    for col in [0:boardCols] do
      fillRect board (if (row + col) % 2 == 0 then dark else light)
        (col * cell) (hud + row * cell) cell cell

  let face ← pinStr "Segoe UI"
  let gfx : Gfx := {
    hwnd, back, board, width, height, cell, hud, dpi
    snakeBrush := ← createSolidBrush (rgb 0x2E 0xA0 0x43)
    headBrush := ← createSolidBrush (rgb 0x56 0xD3 0x64)
    deadBrush := ← createSolidBrush (rgb 0x8B 0x94 0x9E)
    eyeBrush := ← createSolidBrush (rgb 0x0D 0x11 0x17)
    foodBrush := ← createSolidBrush (rgb 0xF8 0x51 0x49)
    shineBrush := ← createSolidBrush (rgb 0xFF 0xA1 0x98)
    panelBrush := ← createSolidBrush (rgb 0x0D 0x11 0x17)
    borderBrush := ← createSolidBrush (rgb 0x56 0xD3 0x64)
    bigFont := ← createFont (sc 36) 700 face
    hudFont := ← createFont (sc 22) 600 face
    smallFont := ← createFont (sc 17) 400 face
  }
  showWindow hwnd 5
  setForegroundWindow hwnd
  return gfx

/-! ## Main loop -/

def main : IO Unit := do
  let gfx ← setup
  let msg ← pinZeros 64 -- a MSG structure (48 bytes) for Windows to fill in
  let mut g := Game.new boardCols boardRows 0 (← IO.rand 0 0x3FFFFFFF)
  let mut held : Array Bool := watchedKeys.map fun _ => false
  let mut last ← IO.monoMsNow
  let mut nextStep := last
  let mut lastDraw := 0
  repeat
    -- Let Windows deliver moves, minimise, close and so on.
    repeat
      if !(← peekMessage msg) then break
      translateMessage msg
      dispatchMessage msg
    if !(← isWindow gfx.hwnd) then break -- the window was closed

    -- Keyboard: act on each key once, at the moment it goes down.
    let mut pressed : Array Nat := #[]
    for i in [0:watchedKeys.size] do
      let vk := watchedKeys[i]!
      let (down, tapped) ← getAsyncKeyState vk
      if (down || tapped) && !held[i]! then
        pressed := pressed.push vk
      held := held.set! i down
    let now ← IO.monoMsNow
    let focused := (← getForegroundWindow) == gfx.hwnd
    -- Pause when the window loses focus or was frozen (e.g. dragged).
    let mut changed := false
    if g.phase == .playing && (!focused || now - last > 400) then
      g := { g with phase := .paused }
      changed := true
    if focused then
      for vk in pressed do
        g := handleKey g vk (← IO.rand 0 0x3FFFFFFF)
        changed := true
    last := now

    -- Move the snake on its own clock, independent of the frame rate.
    if g.phase == .playing then
      if now ≥ nextStep then
        g := g.step (← IO.rand 0 0x3FFFFFFF)
        nextStep := max (nextStep + g.interval) now
        changed := true
    else
      nextStep := now + g.interval

    -- Redraw when something changed, and a few times a second regardless
    -- so the picture comes back after the window was minimised or covered.
    if changed || now - lastDraw ≥ 200 then
      render gfx g
      lastDraw := now
    sleep 5
