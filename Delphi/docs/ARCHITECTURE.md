# Architecture

## Project Structure

```
DyeCSVBuilder.dpr          # Main project file
DyeCSVBuilder.dproj        # Project configuration (MSBuild)
MainForm.pas/.fmx          # Main application window
DyeEntryDialog.pas/.fmx    # Modal dialog for dye entry
FMORecipe.Types.pas        # Core type definitions
FMORecipe.Sets.pas         # Recipe generator (interface-based, primary)
FMORecipe.pas              # Recipe generator (procedural, legacy)
dye_names.txt              # Persisted autocomplete list
sample.csv                 # Example input file
```

## Units

### FMORecipe.Types

Core type definitions:

```pascal
TDyeLocation = (dlFMO, dlCommon);

TDyeEntry = record
  Color: string;           // Dye name (e.g., "FITC", "APC")
  AmountInEachFMO: Double; // Volume in microliters
  Location: TDyeLocation;  // FMO (leave-out) or Common (always present)
end;

TDyeEntryList = TList<TDyeEntry>;
```

### FMORecipe.Sets (Primary Implementation)

Object-oriented implementation using interfaces for clean memory management.

**Key Types:**

| Type | Purpose |
|------|---------|
| `TSetType` | Enum: `stElement`, `stCommonIntermediate`, `stCommonFinal`, `stForward`, `stBackward`, `stFMO` |
| `IElement` | Interface for individual dyes |
| `ISet` | Interface for dye combinations (unions) |
| `TElement` | Concrete element class |
| `TSet` | Concrete set class with left/right inputs |
| `TFMORecipeSimple` | Main recipe builder class |

**Data Flow:**
1. `BuildRecipe()` accepts `TDyeEntryList`
2. Creates `TElement` for each dye
3. Builds forward chain: `F[i] = C + L[0..i-1]`
4. Builds backward chain: `B[i] = L[i+1..m-1]`
5. Combines to create final FMO sets
6. Propagates quantities through the graph

### MainForm

Main UI with:
- `TStringGrid` for dye entries
- `TMemo` for recipe output
- Menu system for file operations
- CRUD buttons for dye management

**Key Methods:**
- `btnGenerateRecipeClick()` - Builds recipe from current entries
- `LoadDyeNames()` / `SaveDyeNames()` - Persist autocomplete list
- `RefreshGrid()` - Sync grid with `FDyeEntries`

### DyeEntryDialog

Modal form for adding/editing dyes:
- `TComboEdit` with filtering for dye name
- `TNumberBox` for amount
- `TComboBox` for location (FMO/Common)

## Class Relationships

```
TfrmMain
  |-- FDyeEntries: TDyeEntryList
  |-- FDyeNames: TStringList (autocomplete)
  |
  +-- uses TFMORecipeSimple
        |-- FElements: TList<IElement>
        |-- FLeaveOutElements: TList<IElement>
        |-- FCommonElements: TList<IElement>
        |-- FFinalSets: TList<ISet>
```

## Memory Management

- `TFMORecipeSimple` uses interfaces (`IElement`, `ISet`) for automatic reference counting
- Forms use standard Delphi ownership model
- `TDyeEntryList` is a generic `TList<TDyeEntry>` (records, no memory management needed)
