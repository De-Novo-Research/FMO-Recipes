# FMO Recipe Algorithm

## Problem Statement

Given:
- **C** = Common elements (always present in every FMO tube)
- **L** = Leave-out elements `[L1, L2, ..., Lm]` (each needs its own FMO)

Goal: Create `m` FMO tubes where tube `Si` contains all elements *except* `Li`.

## Naive Approach

For `m` leave-out elements and `c` common elements:
- Each FMO needs `(m-1) + c` pipetting steps
- Total: `m * ((m-1) + c)` = O(m^2) operations

## Optimized Approach

Uses **forward and backward chains** to reduce operations to O(m).

### Step 1: Common Union

If common elements exist, combine them first:
```
Common = C1 + C2 + ... + Cc
```

### Step 2: Forward Chain

Build cumulative unions from left to right:
```
F1 = Common + L1
F2 = F1 + L2
F3 = F2 + L3
...
F(m-1) = F(m-2) + L(m-1)
```

Note: `F(m-1)` already equals `FMO(-Lm)` (everything except the last element).

### Step 3: Backward Chain

Build cumulative unions from right to left:
```
B(m-1) = Lm              (just the last element)
B(m-2) = L(m-1) + B(m-1)
B(m-3) = L(m-2) + B(m-2)
...
B1 = L2 + B2
```

Note: If no common elements, `B1` already equals `FMO(-L1)`.

### Step 4: Final FMO Tubes

Combine forward and backward chains:
```
FMO(-L1) = Common + B1           (everything except L1)
FMO(-Li) = F(i-1) + B(i)         for i = 2 to m-1
FMO(-Lm) = F(m-1)                (already computed)
```

## Example

Given dyes: `[FITC, PerCP, BUV517, APC]` as FMO, `[BV500, BV222, BV199]` as Common.

**Common Union:**
```
Common = BV500 + BV222 + BV199
```

**Forward Chain (m=4):**
```
F1 = Common + FITC
F2 = F1 + PerCP
F3 = F2 + BUV517    <- This is FMO(-APC)
```

**Backward Chain:**
```
B3 = APC
B2 = BUV517 + B3
B1 = PerCP + B2
```

**Final FMO Tubes:**
```
FMO(-FITC)   = Common + B1
FMO(-PerCP)  = F1 + B2
FMO(-BUV517) = F2 + B3
FMO(-APC)    = F3
```

## Complexity Analysis

| Operation | Naive | Optimized |
|-----------|-------|-----------|
| Common union | - | c-1 |
| Forward chain | - | m-2 |
| Backward chain | - | m-2 |
| Final merges | - | m-2 |
| **Total unions** | O(m^2) | **3m + c - 5** = O(m) |

## Quantity Propagation

After building the recipe graph, quantities flow **backward** from final FMO tubes to source dyes:

1. Each final FMO tube has target quantities per element
2. Quantities propagate through union operations
3. Elements accumulate total required amounts

This allows the recipe to display:
- How much of each dye to pipette at each step
- Total dye volumes needed for the entire experiment
