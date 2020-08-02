# slovarik-mode
Slovarik is a small Russian-English dictionary and minor mode meant for looking up and learning words within Emacs.
It is based on the Wiktionary project and uses simple rules for searching word.

To use it, include `slovarik.el` into a path which Emacs can find.

## Installation

Slovarik can be installed by either just `slovarik.el` or placing the contents of the `src/` directory into a path which Emacs can find.

For example, to install `slovarik.el`, you may run this one-liner:
`$(EMACS=~/.emacs.d; mkdir -p $EMACS/slovarik; cd $EMACS/slovarik; wget https://raw.githubusercontent.com/kirthivaasan/slovarik/master/slovarik.el; cd ..; echo -e "(add-to-list 'load-path \"~/.emacs.d/slovarik/\")\n(load \"slovarik\")" >> init.el;)`

## Usage

Enable the mode by running:

`M-x slovarik-mode`

Point your text cursor and `C-c C-v` to look up a word.

![](demo.gif)
