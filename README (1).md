# Pension Valuation Trainer — PVB Interactive App

## How to run
1. Put `app.R` and `actuarial_engine.R` in the same folder.
2. In R/RStudio: `install.packages(c("shiny","DT","dplyr"))` (one time).
3. Open `app.R` and click **Run App**, or from the console:
   `shiny::runApp("path/to/folder")`

## What it does
- **Left panels:** pick a Life Type (Replacement / Current) and a Benefit Type
  (Retirement / Death / Disability / Vested-Terminated).
- **Top panel:** shows the overall PVB formula (with the selected benefit
  bolded), the benefit-specific formula, and an age-by-age table with every
  component of that formula.
- **Bottom panel:** click any cell in the top table (other than Age) to see
  that component broken down — the sub-formula, the numbers plugged in, and
  (where relevant) the underlying assumption table.

## Files
- `actuarial_engine.R` — all the actuarial math, kept separate from the UI so
  it can be tested/edited independently. This is almost certainly what you'll
  want to edit first.
- `app.R` — the Shiny UI and server logic (layout, table rendering, click
  handling, formula text).

## Important: everything is illustrative
Every assumption in `actuarial_engine.R` — mortality, termination, disability,
and retirement rates, the salary scale, the benefit formula, the discount
rate, the J&S election %, the death benefit multiple — is a made-up
placeholder chosen to produce a sensible-looking teaching example. **Before
using this for anything beyond training, replace these with your firm's
actual assumption tables.** The `build_decrement_table()` function and the
`get_participant()` function are the two places to start.

## Two modeling choices worth flagging to your trainees
1. **Discounting/survival is measured from *entry age*, not the valuation
   date** — this matches your sketch and is standard for an Entry Age Normal
   style PVFB calculation (used to derive normal cost), as opposed to the
   PVB-as-of-valuation-date used directly in the accrued liability. If your
   shop actually wants valuation-date-based PVB, this is a one-line change in
   each `build_*_table()` function (swap `entry_age` for `current_age` in the
   survival/discount calls).
2. **"Joint & Survivor benefit" + "Life annuity"** are modeled as a
   probability-weighted blend (e.g., 70% assumed to elect J&S, 30% assumed to
   elect single life), which is why they sum together in the formula exactly
   as sketched. The election % (`PCT_ELECT_JS`) is a top-of-file constant.

## Top-level formula note
Your sketch had five terms at the top (`Retirement + Death + VR + VD +
Disability`), which looked like it might have been a mid-edit slip since the
left-hand Benefit Type panel only lists four categories. The app uses four
terms (Retirement / Death / Disability / Vested-Terminated) to match the
checkboxes — let me know if you actually wanted a 5-term split (e.g. vested
lives split into "still employed" vs. "deferred") and I can add it back in.
