# buyouts_proj

## Structure
- `data/raw/`        Original, untouched data. Read-only.
- `data/processed/`  Cleaned/derived data, reproducible from raw/.
- `R/`                Pipeline scripts, numbered in run order (01_, 02_, ...).
- `R/functions/`      Reusable functions, sourced by scripts in R/.
- `output/figures/`  Generated plots.
- `output/tables/`   Generated tables.
- `docs/`             Notes, writeups, drafts.

## Conventions
- Use `here::here()` for all file paths, never absolute paths or `setwd()`.
- Never edit files in `data/raw/` directly.
- Scripts in `R/` are numbered by pipeline order.

