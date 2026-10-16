# suggest.hx

Suggestion mode for Helix, like Google Docs. While it is on, your edits become [CriticMarkup](https://fletcher.github.io/MultiMarkdown-6/syntax/critic.html) suggestions instead of changing the text.

| You do | The buffer gets |
| --- | --- |
| type `new` | `{++new++}` |
| delete `old` | `{--old--}` |
| change `old` to `new` | `{~~old~>new~~}` |
| comment | `{>>note<<}` |
| highlight and comment | `{==text==}{>>note<<}` |

Typing inside your own insertion extends it, and deleting there deletes for real. Repeated deletions merge into one `{--…--}`.

## Install

This needs the Steel fork of Helix (`mattwparas/helix`, branch `steel-event-system`). Build it with `cargo xtask steel`.

Run `forge install` from a clone of this repo. Then add this to `~/.config/helix/init.scm`:

```scheme
(require "suggest.hx/suggest.scm")
```

Each new suggestion is tagged with `{>>@$USER<<}`. To use another name, add `(suggest-author "name")`. To turn tagging off, add `(suggest-author "")`.

## Keys

| Key | Command |
| --- | --- |
| `space m t` | `:suggest-mode` toggles suggestion mode for the buffer |
| `space m n`, `]s` | `:suggest-next` selects the next suggestion |
| `space m p`, `[s` | `:suggest-prev` selects the previous suggestion |
| `space m a` / `space m r` | `:suggest-accept` / `:suggest-reject` the suggestion under the cursor |
| `space m A` / `space m R` | `:suggest-accept-all` / `:suggest-reject-all` |
| `space m c` | `:suggest-comment` adds a comment after the selection |
| `space m h` | `:suggest-highlight` highlights the selection and adds a comment |
| `space m s` | `:suggest-list` opens a picker of every suggestion |

The status line shows `SUGGEST` while the mode is on.

## What Gets Tracked

Typing, `ret`, `tab`, backspace, `del`, `C-w`, `A-d`, `C-u` and `C-k` in insert mode. In normal and select mode: `d`, `c`, `p`, `P`, `R`, `o`, `O`, `space p`, `space P`, `space R`, `[space` and `]space`.

These keys are blocked while the mode is on, because their changes can't be tracked: `r`, `~`, `` ` ``, `` A-` ``, `>`, `<`, `=`, `J`, `A-J`, `C-a`, `C-x`, `A-(`, `A-)`, `|`, `!`, `A-!` and insert-mode `C-r`.

## Limits

- Changes from other sources are not tracked. These include surround (`ms`, `mr`, `md`), LSP completions and code actions, formatters, and typed commands such as `:sort`.
- Only the primary cursor is tracked. Tracked keys refuse to run with several cursors, but typed text still lands at every cursor untracked.
- A keymap you define for the file's extension takes precedence over this plugin's buffer keymap.
- CriticMarkup cannot span paragraphs. Edits across a blank line become one suggestion per paragraph, and the break itself stays.
- The plugin API cannot color text, so suggestions show as plain markup.

## Tests

```sh
steel test tests/
```
