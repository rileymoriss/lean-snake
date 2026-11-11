/-
  Direct bindings from Lean to the Win32 API (kernel32, user32, gdi32).

  There is no glue code in another language. Each `@[extern]` declaration
  below names a Windows function, and Lean calls it directly. That works
  because on 64-bit Windows every integer or pointer argument travels in one
  64-bit slot, so a Lean `UInt64` can stand in for any HWND, HDC, int, BOOL
  or pointer, and the result always comes back in one 64-bit register.

  Two details make this sound:

  * Sequencing. Lean treats an `opaque` function as pure, so it would be free
    to merge or drop calls. Every binding therefore takes one extra trailing
    argument, a fresh sequence number (Windows ignores surplus arguments),
    and `call` stores each result in a mutable cell. Both operations are real
    `IO` effects, which pins every call to its place in the program.

  * Memory. Structures and C strings are built as Lean `ByteArray`s. The
    bytes of a `ByteArray` start 24 bytes after the object's address
    (8-byte header, 8-byte size, 8-byte capacity), and Lean never moves an
    object, so that address can be handed to Windows as a pointer for as
    long as the array is kept alive.
-/
namespace Win32

/-! ## Call sequencing -/

initialize seqRef : IO.Ref UInt64 ← IO.mkRef 0
initialize sinkRef : IO.Ref UInt64 ← IO.mkRef 0

/-- Run one foreign call as an `IO` action. -/
@[inline] def call (f : UInt64 → UInt64) : IO UInt64 := do
  let tok ← seqRef.modifyGet fun t => (t, t + 1)
  let r := f tok
  sinkRef.set r
  return r

/-- Low 32 bits of a result (for functions returning `int`, `BOOL`, `DWORD`). -/
def lo32 (v : UInt64) : UInt64 := v &&& 0xFFFFFFFF

/-- Low 16 bits of a result (for functions returning `SHORT`, `ATOM`). -/
def lo16 (v : UInt64) : UInt64 := v &&& 0xFFFF

/-! ## Memory handed to Windows -/

@[noinline] private unsafe def addrOfImpl (b : @& ByteArray) : UInt64 :=
  (ptrAddrUnsafe b).toUInt64 + 24

/-- Address of the first byte stored in a `ByteArray`. -/
@[implemented_by addrOfImpl]
opaque addrOf (b : @& ByteArray) : UInt64

/-- Buffers that must stay valid for the life of the program. -/
initialize pinnedRef : IO.Ref (Array ByteArray) ← IO.mkRef #[]

/-- The buffer used by the foreign call currently in progress. -/
initialize scratchRef : IO.Ref ByteArray ← IO.mkRef ByteArray.empty

/-- Keep a buffer alive forever and return a pointer to its bytes. -/
def pin (b : ByteArray) : IO UInt64 := do
  pinnedRef.modify (·.push b)
  return addrOf b

/-- Keep a buffer alive until the next `scratch` call and return a pointer. -/
def scratch (b : ByteArray) : IO UInt64 := do
  scratchRef.set b
  return addrOf b

/-- A string as zero-terminated bytes. -/
def cstr (s : String) : ByteArray := s.toUTF8.push 0

/-- A permanent zero-terminated copy of a string. -/
def pinStr (s : String) : IO UInt64 := pin (cstr s)

/-- A permanent block of `n` zero bytes for Windows to write into. -/
def pinZeros (n : Nat) : IO UInt64 := do
  let t ← seqRef.get
  let mut b := ByteArray.empty
  for _ in [0:n] do
    b := b.push (t >>> 63).toUInt8
  pin b

/-- Append a little-endian 32-bit value. -/
def pushU32 (b : ByteArray) (v : UInt64) : ByteArray :=
  (((b.push v.toUInt8).push (v >>> 8).toUInt8).push (v >>> 16).toUInt8).push (v >>> 24).toUInt8

/-- Append a little-endian 64-bit value. -/
def pushU64 (b : ByteArray) (v : UInt64) : ByteArray :=
  pushU32 (pushU32 b v) (v >>> 32)

/-- Read a little-endian signed 32-bit value. -/
def readI32 (b : ByteArray) (off : Nat) : Int :=
  let u : Nat :=
    (b.get! off).toNat + (b.get! (off + 1)).toNat * 0x100 +
    (b.get! (off + 2)).toNat * 0x10000 + (b.get! (off + 3)).toNat * 0x1000000
  if u ≥ 0x80000000 then Int.ofNat u - 0x100000000 else Int.ofNat u

/-! ## Raw bindings (last argument is the sequence number) -/

namespace Raw

-- kernel32
@[extern "GetModuleHandleA"] opaque getModuleHandleA (name tok : UInt64) : UInt64
@[extern "GetProcAddress"] opaque getProcAddress (module name tok : UInt64) : UInt64
@[extern "Sleep"] opaque sleep (ms tok : UInt64) : UInt64

-- user32
@[extern "SetProcessDPIAware"] opaque setProcessDPIAware (tok : UInt64) : UInt64
@[extern "GetSystemMetrics"] opaque getSystemMetrics (index tok : UInt64) : UInt64
@[extern "LoadCursorA"] opaque loadCursorA (inst name tok : UInt64) : UInt64
@[extern "RegisterClassA"] opaque registerClassA (wndClass tok : UInt64) : UInt64
@[extern "AdjustWindowRectEx"] opaque adjustWindowRectEx (rect style menu exStyle tok : UInt64) : UInt64
@[extern "CreateWindowExA"] opaque createWindowExA
  (exStyle cls title style x y w h parent menu inst param tok : UInt64) : UInt64
@[extern "ShowWindow"] opaque showWindow (hwnd cmd tok : UInt64) : UInt64
@[extern "SetForegroundWindow"] opaque setForegroundWindow (hwnd tok : UInt64) : UInt64
@[extern "GetForegroundWindow"] opaque getForegroundWindow (tok : UInt64) : UInt64
@[extern "IsWindow"] opaque isWindow (hwnd tok : UInt64) : UInt64
@[extern "PeekMessageA"] opaque peekMessageA (msg hwnd min max remove tok : UInt64) : UInt64
@[extern "TranslateMessage"] opaque translateMessage (msg tok : UInt64) : UInt64
@[extern "DispatchMessageA"] opaque dispatchMessageA (msg tok : UInt64) : UInt64
@[extern "GetAsyncKeyState"] opaque getAsyncKeyState (vk tok : UInt64) : UInt64
@[extern "GetDC"] opaque getDC (hwnd tok : UInt64) : UInt64
@[extern "ReleaseDC"] opaque releaseDC (hwnd dc tok : UInt64) : UInt64

-- gdi32
@[extern "GetDeviceCaps"] opaque getDeviceCaps (dc index tok : UInt64) : UInt64
@[extern "CreateCompatibleDC"] opaque createCompatibleDC (dc tok : UInt64) : UInt64
@[extern "CreateCompatibleBitmap"] opaque createCompatibleBitmap (dc w h tok : UInt64) : UInt64
@[extern "SelectObject"] opaque selectObject (dc obj tok : UInt64) : UInt64
@[extern "GetStockObject"] opaque getStockObject (index tok : UInt64) : UInt64
@[extern "CreateSolidBrush"] opaque createSolidBrush (color tok : UInt64) : UInt64
@[extern "PatBlt"] opaque patBlt (dc x y w h rop tok : UInt64) : UInt64
@[extern "RoundRect"] opaque roundRect (dc l t r b ew eh tok : UInt64) : UInt64
@[extern "Ellipse"] opaque ellipse (dc l t r b tok : UInt64) : UInt64
@[extern "BitBlt"] opaque bitBlt (dst x y w h src sx sy rop tok : UInt64) : UInt64
@[extern "CreateFontA"] opaque createFontA
  (height width esc orient weight italic underline strike charset
   outPrec clipPrec quality pitch face tok : UInt64) : UInt64
@[extern "SetBkMode"] opaque setBkMode (dc mode tok : UInt64) : UInt64
@[extern "SetTextColor"] opaque setTextColor (dc color tok : UInt64) : UInt64
@[extern "SetTextAlign"] opaque setTextAlign (dc align tok : UInt64) : UInt64
@[extern "TextOutA"] opaque textOutA (dc x y str len tok : UInt64) : UInt64

end Raw

/-! ## `IO` wrappers -/

/-- Convert a pixel count or other small number to an argument. -/
def u (n : Nat) : UInt64 := n.toUInt64

def getModuleHandle (name : UInt64) : IO UInt64 := call (Raw.getModuleHandleA name)
def getProcAddress (module name : UInt64) : IO UInt64 := call (Raw.getProcAddress module name)
def sleep (ms : Nat) : IO Unit := discard <| call (Raw.sleep (u ms))

def setProcessDPIAware : IO Unit := discard <| call Raw.setProcessDPIAware
def getSystemMetrics (index : Nat) : IO Nat := do
  return (lo32 (← call (Raw.getSystemMetrics (u index)))).toNat
def loadCursor (inst name : UInt64) : IO UInt64 := call (Raw.loadCursorA inst name)
def registerClass (wndClass : UInt64) : IO UInt64 := do
  return lo16 (← call (Raw.registerClassA wndClass))
def adjustWindowRectEx (rect style : UInt64) : IO Unit :=
  discard <| call (Raw.adjustWindowRectEx rect style 0 0)
def createWindowEx (cls title style : UInt64) (x y w h : Nat) (inst : UInt64) : IO UInt64 :=
  call (Raw.createWindowExA 0 cls title style (u x) (u y) (u w) (u h) 0 0 inst 0)
def showWindow (hwnd : UInt64) (cmd : Nat) : IO Unit :=
  discard <| call (Raw.showWindow hwnd (u cmd))
def setForegroundWindow (hwnd : UInt64) : IO Unit :=
  discard <| call (Raw.setForegroundWindow hwnd)
def getForegroundWindow : IO UInt64 := call Raw.getForegroundWindow
def isWindow (hwnd : UInt64) : IO Bool := do
  return lo32 (← call (Raw.isWindow hwnd)) != 0
/-- Take the next waiting window message, if any. -/
def peekMessage (msg : UInt64) : IO Bool := do
  return lo32 (← call (Raw.peekMessageA msg 0 0 0 1)) != 0
def translateMessage (msg : UInt64) : IO Unit := discard <| call (Raw.translateMessage msg)
def dispatchMessage (msg : UInt64) : IO Unit := discard <| call (Raw.dispatchMessageA msg)
/-- `(held down now, pressed since the last query)` for a virtual key. -/
def getAsyncKeyState (vk : Nat) : IO (Bool × Bool) := do
  let s ← call (Raw.getAsyncKeyState (u vk))
  return ((s &&& 0x8000) != 0, (s &&& 1) != 0)
def getDC (hwnd : UInt64) : IO UInt64 := call (Raw.getDC hwnd)
def releaseDC (hwnd dc : UInt64) : IO Unit := discard <| call (Raw.releaseDC hwnd dc)

def getDeviceCaps (dc : UInt64) (index : Nat) : IO Nat := do
  return (lo32 (← call (Raw.getDeviceCaps dc (u index)))).toNat
def createCompatibleDC (dc : UInt64) : IO UInt64 := call (Raw.createCompatibleDC dc)
def createCompatibleBitmap (dc : UInt64) (w h : Nat) : IO UInt64 :=
  call (Raw.createCompatibleBitmap dc (u w) (u h))
def selectObject (dc obj : UInt64) : IO Unit := discard <| call (Raw.selectObject dc obj)
def getStockObject (index : Nat) : IO UInt64 := call (Raw.getStockObject (u index))
def createSolidBrush (color : UInt64) : IO UInt64 := call (Raw.createSolidBrush color)

/-- A colour in the form GDI expects (0x00BBGGRR). -/
def rgb (r g b : Nat) : UInt64 := u (r + g * 0x100 + b * 0x10000)

/-- Fill a rectangle with a brush. -/
def fillRect (dc brush : UInt64) (x y w h : Nat) : IO Unit := do
  selectObject dc brush
  discard <| call (Raw.patBlt dc (u x) (u y) (u w) (u h) 0x00F00021)

/-- Fill a rounded rectangle covering pixels `[x, x+w) × [y, y+h)`.
    The device context must have the null pen selected. -/
def fillRoundRect (dc brush : UInt64) (x y w h corner : Nat) : IO Unit := do
  selectObject dc brush
  discard <| call (Raw.roundRect dc (u x) (u y) (u (x + w + 1)) (u (y + h + 1)) (u corner) (u corner))

/-- Fill an ellipse inscribed in `[x, x+w) × [y, y+h)`. -/
def fillEllipse (dc brush : UInt64) (x y w h : Nat) : IO Unit := do
  selectObject dc brush
  discard <| call (Raw.ellipse dc (u x) (u y) (u (x + w + 1)) (u (y + h + 1)))

/-- Copy a `w × h` block from the top-left of `src` to the top-left of `dst`. -/
def blit (dst src : UInt64) (w h : Nat) : IO Unit :=
  discard <| call (Raw.bitBlt dst 0 0 (u w) (u h) src 0 0 0x00CC0020)

/-- Create a ClearType font of the given pixel height and weight. -/
def createFont (pixelHeight weight : Nat) (face : UInt64) : IO UInt64 :=
  -- A negative height selects by character height; pass it as two's complement.
  call (Raw.createFontA (0 - u pixelHeight) 0 0 0 (u weight) 0 0 0 0 0 0 5 0 face)

def setTransparentText (dc : UInt64) : IO Unit := discard <| call (Raw.setBkMode dc 1)

inductive Align where
  | left | right | center

def Align.code : Align → UInt64
  | .left => 0
  | .right => 2
  | .center => 6

/-- Draw ASCII text. `x` is the left edge, right edge or centre, per `align`. -/
def drawText (dc font color : UInt64) (align : Align) (x y : Nat) (s : String) : IO Unit := do
  selectObject dc font
  discard <| call (Raw.setTextColor dc color)
  discard <| call (Raw.setTextAlign dc align.code)
  let bytes := cstr s
  let p ← scratch bytes
  discard <| call (Raw.textOutA dc (u x) (u y) p (u (bytes.size - 1)))

end Win32
