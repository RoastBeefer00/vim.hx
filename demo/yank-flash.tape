# vim.hx yank flash demo
# Requirements: helix-steel with vim.hx loaded
# The flash briefly highlights the yanked region in ui.selection before collapsing.
# Run from this directory: vhs yank-flash.tape

Output yank-flash.gif

Set Shell "bash"
Set FontSize 14
Set Width 1000
Set Height 585
Set Theme "Catppuccin Mocha"
Set TypingSpeed 80ms

Type "hx sample.py"
Enter
Sleep 2s
Type ":showkeys-toggle"
Enter
Sleep 200ms

# ── yiw : yank inner word, flash the word ──────────────────────────────────
# Cursor starts at line 1 col 1 ("def")
# Move to "value" on line 1
Type "fv"
Sleep 300ms

Type "yiw"
Sleep 700ms

# ── yy : yank whole line, flash the line ───────────────────────────────────
# Move to line 3: "    message = "done""
Type "2j"
Sleep 300ms

Type "yy"
Sleep 700ms

# ── ya( / yank around parens ───────────────────────────────────────────────
# Move to line 9: "    output = calculate(10, 3)"
Type "6j"
Sleep 300ms
Type "f("
Sleep 200ms

Type "ya("
Sleep 700ms

# ── y$ : yank to end of line ──────────────────────────────────────────────
# Move to line 2: "    result = value * multiplier"
Type "7k"
Sleep 300ms
Type "fw"
Sleep 200ms

Type "y$"
Sleep 700ms

# Hold on the final state before GIF loops
Sleep 1500ms

Type ":q!"
Enter
