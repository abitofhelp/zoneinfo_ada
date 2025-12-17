# Zoneinfo Library Documentation

**Version:** 1.1.0<br>
**Date:** 2025-12-16<br>
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>
**Status:** Released

---

## Overview

Zoneinfo is a timezone-aware datetime library for Ada 2022, providing high-level timezone calculations and duration operations. Built on the TZif library for IANA timezone database access, it delivers timezone-safe datetime manipulation for both desktop and embedded platforms.

**Key Capabilities:**

- Timezone-aware datetime types (Instant, Zoned, Civil)
- Duration arithmetic with overflow protection
- ISO 8601 string formatting and parsing
- IANA timezone database integration via TZif
- Desktop platform support (Linux/macOS/Windows)
- Embedded-safe design (bounded types, static allocation)
- SPARK-verified Domain and Application layers

**Test Coverage:** 335 unit + 154 integration = 489 total tests (all passing)

---

## Quick Navigation

### Getting Started

- **[Quick Start Guide](./quick_start.md)** - Installation, basic usage, and first programs
- **[README](../README.md)** - Project overview and feature list

### Formal Documentation

- **[Software Requirements Specification](./formal/software_requirements_specification.md)** - Functional and non-functional requirements
- **[Software Design Specification](./formal/software_design_specification.md)** - Architecture, design patterns, and implementation
- **[Software Test Guide](./formal/software_test_guide.md)** - Test strategy, execution, and writing new tests

### Developer Guides

- **[TZif API Mapping](./guides/tzif_api_mapping.md)** - How Zoneinfo operations map to TZif

### Reference

- **[CHANGELOG](../CHANGELOG.md)** - Release history and version details

---

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                     Zoneinfo.API (Facade)                       │
│           Re-exports Domain Types + Composition Roots            │
├──────────────┬──────────────┬──────────────┬───────────────────┤
│ API.Desktop  │ API.Discovery│  API.Format  │    API.Parse      │
│ (Desktop I/O)│ (Zone Search)│  (ISO 8601)  │  (String Parse)   │
├──────────────┴──────────────┴──────────────┴───────────────────┤
│              API.Operations (SPARK-Safe Pure Ops)               │
├─────────────────────────────────────────────────────────────────┤
│                      Application Layer                          │
│          Use Cases │ Ports (Clock, Timezone, Writer)            │
├─────────────────────────────────────────────────────────────────┤
│                    Infrastructure Layer                         │
│       Adapters: Desktop_Clock │ TZif │ Discovery │ Console      │
├─────────────────────────────────────────────────────────────────┤
│                       Domain Layer                              │
│  Value Objects: Instant │ Zoned │ Civil │ Duration │ Zone_ID    │
│  Collections: Zone_List │ Search_Results (bounded arrays)       │
│  Error: Result[T] (7 operations) │ Error_Type │ Unit            │
└─────────────────────────────────────────────────────────────────┘

Dependency Direction: ▲ (Lower layers independent of upper layers)
SPARK Verification: Domain + Application layers (--mode=check)
```

**Design Principles:**

- **Hexagonal Architecture** - Ports & adapters with dependency inversion
- **Clean Architecture** - Domain-centric with infrastructure abstraction
- **Functional Error Handling** - Result monad (no exceptions)
- **SPARK Verification** - Domain + Application layers formally verified
- **Embedded Safety** - No heap allocations, bounded types throughout

---

## Domain Value Objects

| Type | Description | Key Operations |
|------|-------------|----------------|
| **Instant** | Absolute moment (epoch nanoseconds) | From_Unix_Epoch, Add, Subtract, Diff |
| **Zoned** | Instant + timezone context | Create, To_Instant, Get_Zone, With_Zone |
| **Civil** | Calendar components (Y/M/D/H/M/S/ns) | Create, Get_Year..Get_Nanosecond |
| **Duration_Type** | Time span (nanosecond precision) | From_Seconds, Add, Subtract, Negate |
| **Zone_ID** | IANA timezone identifier | From_String, To_String, UTC, Is_UTC |
| **Source_Info** | Timezone database metadata | Path, Version, ULID |

**Bounded Collections (SPARK-compatible):**

| Type | Capacity | Purpose |
|------|----------|---------|
| **Zone_List** | Max_Zone_List_Size (750) | List_All_Zones results |
| **Search_Results** | Max_Search_Results (100) | Find_By_* results |

---

## API Operations

### Discovery API (Zoneinfo.API.Discovery)

**Source Management:**
- `Discover_Sources` - Scan paths for IANA timezone databases
- `Load_Source` - Load timezone data from a path
- `Validate_Source` - Verify timezone source integrity

**Timezone Queries:**
- `Find_My_Id` - Get local system timezone
- `Get_Version` - Get timezone database version
- `List_All_Zones` - Returns `Zone_List_Result.Result` (bounded array)

**Pattern-Based Search:** (all return `Search_Results_Result.Result`)
- `Find_By_Pattern` - Substring search (e.g., "York" matches "America/New_York")
- `Find_By_Region` - Search by region (e.g., "America")
- `Find_By_Regex` - Regular expression search

**Usage Example:**
```ada
Zones_Result := List_All_Zones (Source);
if Zone_List_Result.Is_Ok (Zones_Result) then
   for I in 1 .. Zone_List_Result.Value (Zones_Result).Count loop
      Process (Zone_List_Result.Value (Zones_Result).Items (I));
   end loop;
end if;
```

### Format API (Zoneinfo.API.Format)

**Civil Formatting:**
- `To_ISO_8601` - "2025-12-16T14:30:00.123456789"
- `To_ISO_8601_With_Offset` - "2025-12-16T14:30:00-05:00"
- `To_ISO_8601_With_Zone` - "2025-12-16T14:30:00[America/New_York]"
- `To_ISO_8601_Full` - "2025-12-16T14:30:00-05:00[America/New_York]"
- `To_ISO_Date` - "2025-12-16"
- `To_ISO_Time` - "14:30:00.123456789"

**Duration Formatting:**
- `To_ISO_Duration` - "PT1H30M45S" (ISO 8601)
- `To_Human_Duration` - "1h 30m 45s" (human-readable)

### Parse API (Zoneinfo.API.Parse)

**Civil Parsing:**
- `From_ISO_8601` - Parse "2025-12-16T14:30:00"
- `From_ISO_8601_With_Offset` - Parse "2025-12-16T14:30:00-05:00"
- `From_ISO_8601_With_Zone` - Parse "2025-12-16T14:30:00[America/New_York]"
- `From_ISO_8601_Full` - Parse full ISO 8601 with offset and zone
- `From_ISO_Date` - Parse "2025-12-16"
- `From_ISO_Time` - Parse "14:30:00"

**Duration Parsing:**
- `From_ISO_Duration` - Parse "PT1H30M45S"
- `From_Human_Duration` - Parse "1h 30m 45s"
- `Parse_Offset` - Parse "+05:00" or "Z"

### Operations API (Zoneinfo.API.Operations)

**SPARK-Safe Pure Operations:**
- `Add` / `"+"` - Add duration to instant
- `Subtract` / `"-"` - Subtract duration from instant
- `Diff` / `"-"` - Calculate duration between instants
- Duration arithmetic: `+`, `-`, unary `-`

---

## Error Handling

All operations return `Result[T]` - no exceptions are raised.

**Error Kinds:**

| Kind | Description | Example |
|------|-------------|---------|
| `Validation_Error` | Invalid input | Malformed datetime string |
| `Timezone_Error` | Timezone operation failed | Unknown zone ID |
| `Overflow_Error` | Arithmetic overflow | Instant out of range |
| `Ambiguous_Time_Error` | DST fall-back ambiguity | 1:30 AM on DST end |
| `Gap_Time_Error` | DST spring-forward gap | 2:30 AM on DST start |
| `IO_Error` | I/O operation failed | Cannot read timezone file |
| `Internal_Error` | Internal library error | Unexpected state |

**Result Operations (Domain.Error.Result):**

```ada
Ok (Value)      -- Construct success
Error (Kind, Message)  -- Construct error
Is_Ok (R)       -- Check success
Is_Error (R)    -- Check failure
Value (R)       -- Extract value (Pre: Is_Ok)
Error_Info (R)  -- Extract error (Pre: Is_Error)
From_Error (E)  -- Convert Error_Type to Result
```

---

## Platform Support

| Platform | Status | Clock Source | Timezone Discovery |
|----------|--------|--------------|-------------------|
| **Linux** | Full | `Ada.Calendar.Clock` | `/etc/localtime`, `/usr/share/zoneinfo` |
| **macOS** | Full | `Ada.Calendar.Clock` | `/etc/localtime`, `/usr/share/zoneinfo` |
| **Windows** | Full | `Ada.Calendar.Clock` | Registry + Windows API |
| **Embedded** | Custom | User-provided clock port | User-provided TZif source |

---

## Dependencies

| Crate | Version | Purpose |
|-------|---------|---------|
| **functional** | ^4.0.0 | Result/Option/Try monads |
| **tzif** | ^3.0.3 | IANA timezone database access |
| **gnatcoll** | ^25.0.0 | GNAT Components Collection |

**Compiler:** GNAT 14+ (Ada 2022)

---

## Documentation Structure

```
docs/
├── index.md                    # This file
├── quick_start.md              # Get started in minutes
├── formal/
│   ├── software_requirements_specification.md
│   ├── software_design_specification.md
│   └── software_test_guide.md
└── guides/
    └── tzif_api_mapping.md
```

---

## Need Help?

- **Getting started?** → [Quick Start Guide](./quick_start.md)
- **Running tests?** → [Software Test Guide](./formal/software_test_guide.md)
- **Understanding errors?** → [Quick Start: Error Handling](./quick_start.md#error-handling)
- **Architecture questions?** → [Software Design Specification](./formal/software_design_specification.md)

---

**License:** BSD-3-Clause<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.
