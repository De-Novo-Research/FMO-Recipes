# FMO Recipe Generator

Tools for generating optimized Fluorescence Minus One (FMO) control mixing recipes for flow cytometry experiments.

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Platform](https://img.shields.io/badge/platform-Windows%20%7C%20macOS-blue.svg)]()

## Overview

FMO (Fluorescence Minus One) controls are essential for accurate gating in multicolor flow cytometry. Each FMO tube contains all fluorophores in a panel *except one*, helping identify gating boundaries by accounting for spectral overlap.

This repository provides **two implementations** of an optimized mixing protocol generator that minimizes pipetting steps using a forward/backward chain algorithm, reducing preparation time and reagent waste:

| Implementation | Description | Best For |
|----------------|-------------|----------|
| **Delphi GUI** | Cross-platform desktop app with visual interface | Interactive use, beginners |
| **Racket CLI** | Command-line tool for batch processing | Scripting, automation, pipelines |

Both implementations use the same algorithm and produce equivalent results.

---

## Delphi GUI Application

A cross-platform desktop application built with FireMonkey (FMX).

### Features

- Add, edit, and delete dye entries with color name, volume, and location
- Designate dyes as "FMO" (leave-out) or "Common Backbone" (always present)
- Generate optimized mixing recipes with step-by-step instructions
- Calculate total reagent requirements
- Load/save dye configurations as CSV files
- Export recipes as text files
- Autocomplete for previously used dye names

### Installation

#### Pre-built Binaries

Download the latest release for your platform from the [Releases](../../releases) page.

#### Building from Source

**Requirements:**
- Embarcadero Delphi RAD Studio (10.4+ recommended)
- FireMonkey (FMX) framework

**Steps:**
1. Clone the repository
2. Open `Delphi/DyeCSVBuilder.dproj` in RAD Studio
3. Select target platform (Win32, Win64, OSX64, or OSXARM64)
4. Build and run

### Usage

1. **Add dyes** - Click "Add" to enter each fluorophore with:
   - Color/name (e.g., "FITC", "APC", "BV785")
   - Volume per FMO tube (in microliters)
   - Location: "Make FMO" or "Common Backbone"

2. **Generate recipe** - Click Build > Build Recipe (or use the menu)

3. **Follow the recipe** - The output provides:
   - Labelling steps (which tubes to prepare)
   - Mixing steps (what to combine and in what order)
   - Final FMO tube compositions for verification

4. **Save/export** - Save your dye list as CSV for reuse, or export the recipe as text

### Input Format (Delphi)

```csv
Dye,amount,FMO
FITC,10,True
PerCP,23,True
BUV395,66,True
APC,25,True
eFluor 450,23,False
BV785,76,False
PerCP-Cy5.5,100,False
```

| Column | Description |
|--------|-------------|
| `Dye` | Dye/fluorophore name |
| `Amount` | Volume per FMO tube (microliters) |
| `FMO` | `True` = leave-out dye (make FMO), `False` = common backbone (always present) |

---

## Racket CLI Tool

A command-line interface for batch processing and automation.

### Installation

**Requirements:**
- [Racket](https://racket-lang.org/) (v8.0+ recommended)

**Steps:**
1. Clone the repository
2. Navigate to the `Racket Algo` directory

### Usage

```bash
racket typed-cli.rkt input.csv output.csv
```

### Input Format (Racket)

```csv
Dye,Amount,FMO
FITC,10,yes
PerCP,23,yes
BUV395,66,yes
APC,25,yes
eFluor 450,23,no
BV785,76,no
PerCP-Cy5.5,100,no
```

| Column | Description |
|--------|-------------|
| `Dye` | Dye/fluorophore name |
| `Amount` | Volume per FMO tube (microliters) |
| `FMO` | `true`/`yes`/`1`/`y` = FMO dye, anything else = common backbone |

### Output Format (Racket)

The CLI generates a CSV file with recipe steps:

```csv
result_set,input1_set,input1_volume,input2_set,input2_volume,dye_compositions
L-FC1,C-core3,199,APC,25,"APC|25;BV785|76;eFluor 450|23;PerCP-Cy5.5|100"
...

TOTAL QUANTITIES REQUIRED
dye,total_quantity
APC,75
BUV395,198
...
```

---

## Requirements

Both implementations require:
- Minimum 3 total dyes
- At least 3 dyes marked as "leave out" / "Make FMO"

---

## Sample Output (Delphi GUI)

```
FMO RECIPE
==========

Final Dye Amount Per FMO:
-----------
  BV785: 76.00 uL (common)
  eFluor 450: 23.00 uL (common)
  PerCP-Cy5.5: 100.00 uL (common)
  APC: 25.00 uL (FMO)
  BUV395: 66.00 uL (FMO)
  FITC: 10.00 uL (FMO)
  PerCP: 23.00 uL (FMO)

Required Reagents:
-------------

  APC: 75.00 uL
  BUV395: 198.00 uL
  BV785: 304.00 uL
  eFluor 450: 92.00 uL
  FITC: 30.00 uL
  PerCP: 69.00 uL
  PerCP-Cy5.5: 400.00 uL
  9 tubes

Labelling Steps:
-------------

  Step 1: Label a tube as "Common"
  Step 2: Label a tube as "F1"
  Step 3: Label a tube as "F2"
  Step 4: Label a tube as "B1"
  Step 5: Label a tube as "B2"
  Step 6: Label a tube as "FMO (-APC)"
  Step 7: Label a tube as "FMO (-BUV395)"
  Step 8: Label a tube as "FMO (-FITC)"
  Step 9: Label a tube as "FMO (-PerCP)"

Mixing Steps:
-------------

  Step 1: Add to tube Common:
    BV785: 304.00 uL
    eFluor 450: 92.00 uL
    PerCP-Cy5.5: 400.00 uL

  Step 2: Mix: FITC (20.00 uL) + PerCP (46.00 uL) into tube B2
  Contains:
    FITC: 20.00 uL
    PerCP: 46.00 uL

  Step 3: Mix: BUV395 (66.00 uL) + B2 (33.00 uL) into tube B1
  Contains:
    BUV395: 66.00 uL
    FITC: 10.00 uL
    PerCP: 23.00 uL

  Step 4: Mix: Common (199.00 uL) + B1 (99.00 uL) into tube FMO (-APC)
  Contains:
    BV785: 76.00 uL
    eFluor 450: 23.00 uL
    PerCP-Cy5.5: 100.00 uL
    BUV395: 66.00 uL
    FITC: 10.00 uL
    PerCP: 23.00 uL

  Step 5: Mix: Common (597.00 uL) + APC (75.00 uL) into tube F1
  Contains:
    BV785: 228.00 uL
    eFluor 450: 69.00 uL
    PerCP-Cy5.5: 300.00 uL
    APC: 75.00 uL

  Step 6: Mix: F1 (224.00 uL) + B2 (33.00 uL) into tube FMO (-BUV395)
  Contains:
    BV785: 76.00 uL
    eFluor 450: 23.00 uL
    PerCP-Cy5.5: 100.00 uL
    APC: 25.00 uL
    FITC: 10.00 uL
    PerCP: 23.00 uL

  Step 7: Mix: F1 (448.00 uL) + BUV395 (132.00 uL) into tube F2
  Contains:
    BV785: 152.00 uL
    eFluor 450: 46.00 uL
    PerCP-Cy5.5: 200.00 uL
    APC: 50.00 uL
    BUV395: 132.00 uL

  Step 8: Mix: F2 (290.00 uL) + PerCP (23.00 uL) into tube FMO (-FITC)
  Contains:
    BV785: 76.00 uL
    eFluor 450: 23.00 uL
    PerCP-Cy5.5: 100.00 uL
    APC: 25.00 uL
    BUV395: 66.00 uL
    PerCP: 23.00 uL

  Step 9: Mix: F2 (290.00 uL) + FITC (10.00 uL) into tube FMO (-PerCP)
  Contains:
    BV785: 76.00 uL
    eFluor 450: 23.00 uL
    PerCP-Cy5.5: 100.00 uL
    APC: 25.00 uL
    BUV395: 66.00 uL
    FITC: 10.00 uL

FMO Control Tubes:
------------------
  FMO (-APC):
    BV785: 76.00 uL
    eFluor 450: 23.00 uL
    PerCP-Cy5.5: 100.00 uL
    BUV395: 66.00 uL
    FITC: 10.00 uL
    PerCP: 23.00 uL

  FMO (-BUV395):
    BV785: 76.00 uL
    eFluor 450: 23.00 uL
    PerCP-Cy5.5: 100.00 uL
    APC: 25.00 uL
    FITC: 10.00 uL
    PerCP: 23.00 uL

  FMO (-FITC):
    BV785: 76.00 uL
    eFluor 450: 23.00 uL
    PerCP-Cy5.5: 100.00 uL
    APC: 25.00 uL
    BUV395: 66.00 uL
    PerCP: 23.00 uL

  FMO (-PerCP):
    BV785: 76.00 uL
    eFluor 450: 23.00 uL
    PerCP-Cy5.5: 100.00 uL
    APC: 25.00 uL
    BUV395: 66.00 uL
    FITC: 10.00 uL
```

---

## Algorithm

Both implementations use a forward/backward chain algorithm to minimize mixing operations:

1. **Common pool** - Combine all "Common Backbone" dyes once
2. **Forward chain** - Build cumulative mixtures left-to-right: `F[i] = F[i-1] + L[i]`
3. **Backward chain** - Build cumulative mixtures right-to-left: `B[i] = L[i+1] + B[i+1]`
4. **Final FMOs** - Combine appropriate forward and backward chains

This reduces complexity from O(m²) to O(m) mixing operations, where m is the number of FMO dyes.

See [Delphi/docs/ALGORITHM.md](Delphi/docs/ALGORITHM.md) for detailed explanation.

---

## Repository Structure

```
├── Delphi/                    # GUI application
│   ├── DyeCSVBuilder.dproj    # Project file
│   ├── MainForm.pas           # Main window
│   ├── DyeEntryDialog.pas     # Dye entry dialog
│   ├── FMORecipe.Sets.pas     # Recipe algorithm
│   ├── FMORecipe.Types.pas    # Type definitions
│   └── docs/                  # Documentation
│       ├── README.md
│       ├── ARCHITECTURE.md
│       └── ALGORITHM.md
└── Sample Input/         # Different example inputs
│   ├── sample.csv             # Example input
└── Racket/               # CLI tool
    ├── typed-cli.rkt          # Command-line interface
```

---

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

---

## License

This project is licensed under the GNU 3.0 License

---
