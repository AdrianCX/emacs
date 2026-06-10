# TLDR — Commands

| Key     | Command                  | What it does |
|---------|--------------------------|--------------|
| `C-d`   | `load-project`           | Pick the dir holding `cscope.files` (also `M-d`). Run this first. |
| `C-c t` | `ctags-build`            | Build/reload the `TAGS` index from `cscope.files`. |
| `M-.`   | `xref-find-definitions`  | Jump to definition of symbol under point. |
| `M-,`   | `xref-go-back`           | Jump back. |
| `C-c r` | `ctags-find-references`  | Grep a literal symbol across the indexed files (hit list). |
| `C-c s` | `ctags-search`           | Regexp text search across the indexed files (hit list). |
| `C-c S` | `tags-search`            | Built-in search over the TAGS db; `M-0 M-,` for next match. |
| `C-w`   | `ido-choose-from-cscope` | Open a file from `cscope.files` by name (also `M-w`). |
| `C-e`   | `ido-choose-from-recentf`| Open a recent file (also `M-e`). |
| `C-f5`  | `cscope-find-this-symbol`| cscope symbol search. |

Typical flow: `C-d` → `C-c t` → then `M-.`, `C-c r`, `C-c s`.
