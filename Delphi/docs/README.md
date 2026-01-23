# DyeCSVBuilder - FMO Recipe Generator

A Delphi FireMonkey (FMX) application for generating optimized Fluorescence Minus One (FMO) control recipes for flow cytometry experiments.

## What is FMO?

FMO (Fluorescence Minus One) controls are used in flow cytometry to identify gating boundaries. Each FMO tube contains all fluorescent dyes in a panel *except one*, helping to account for spectral overlap and background fluorescence.

## Purpose

When you have N dyes that need FMO controls, you need N separate tubes, each missing one dye. Naive preparation would require pipetting each dye into each tube individually. This application generates an **optimized mixing recipe** that minimizes pipetting steps by:

1. Creating intermediate "pools" of dyes
2. Building forward and backward chains
3. Combining chains to produce final FMO tubes

## Features

- Add/edit/delete dye entries with color name, amount (uL), and location (FMO or Common Backbone)
- Load/save dye configurations as CSV files
- Generate optimized FMO mixing recipes
- Export recipes as text files
- Autocomplete for dye names (persisted in `dye_names.txt`)

## Dye Locations

- **Make FMO**: Dyes that will have their own FMO control (leave-out dyes)
- **Common Backbone**: Dyes present in ALL FMO tubes (never left out)

## Requirements

- At least 3 total dyes
- At least 3 dyes marked as "Make FMO"

## File Formats

### Input CSV
```csv
color,amount,FMO
FITC,10,1
PerCP,23,1
BV500,23,0
```

### Output Recipe
Text file containing:
- Input dye summary
- Required reagent totals
- Labelling steps (which tubes to prepare)
- Mixing steps (what to combine into each tube)
- Final FMO control tube compositions

## Building

Open `DyeCSVBuilder.dproj` in Delphi RAD Studio. Supports:
- Windows (Win32/Win64)
- macOS (OSX64/OSXARM64)
