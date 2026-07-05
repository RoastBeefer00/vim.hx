# vim.hx

Vim bindings for helix, using Steel.

## Installation

```shell
forge pkg install --git https://github.com/mattwparas/vim.hx.git
````

## Usage

```scheme
(require "vim-hx/init.scm")

;; Add this to your init.scm
(set-vim-keybindings!)
```

## Configuration

After calling `(set-vim-keybindings!)`, you can tune these variables:

```scheme
;; Duration of the yank highlight flash in milliseconds (default: 350)
(set! *yank-flash-delay* 350)

;; Theme scope used for the yank highlight (default: "ui.selection")
(set! *yank-flash-scope* "ui.selection")
```

## Contributors

Besides myself:

* RoastBeefer00
* nacl-gb3
