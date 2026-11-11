# Snake in Lean 4

A windowed Snake game written entirely in Lean 4. There is no C, no script and
no GUI library: Lean calls the Windows API (user32 / gdi32) directly.

## Play

Double-click `.lake\build\bin\snake.exe`, or from this folder:

```
lake exe snake
```

| Key | Action |
| --- | --- |
| Arrow keys / W A S D | Steer (the first press starts the game) |
| Space / Enter | Pause, resume, play again after game over |
| P / Esc | Pause, resume |
| R | Restart |

Eat the red food to grow and score. Hitting a wall or yourself ends the game.
The snake speeds up as it grows, and the game pauses itself when the window
loses focus.

## Build

Requires Windows 10/11 (64-bit) and [elan](https://github.com/leanprover/elan),
which installs the Lean version pinned in `lean-toolchain`.

```
lake build
```

## Files

- `Snake/Game.lean` - pure game rules (movement, food, collisions, speed).
- `Snake/Win32.lean` - bindings from Lean to the Windows API.
- `Main.lean` - window creation, keyboard input, drawing and the main loop.
