# TZif API Mapping Guide

**Doc Version:** 1.1.1<br>
**Applies to zoneinfo_ada:** ^1.1<br>
**Last Updated:** 2026-04-26<br>
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2026 Michael Gardner, A Bit of Help, Inc.<br>
**Status:** Released

---

## Overview

This document describes how Zoneinfo operations map to the underlying TZif library. Zoneinfo provides a higher-level datetime API built on top of TZif's timezone data operations.

**Key Principle:** Zoneinfo wraps TZif with proper types - it does not expose TZif directly to consumers.

---

## Table of Contents

- [Architecture Relationship](#architecture-relationship)
- [Type Mapping](#type-mapping)
- [API Mapping Tables](#api-mapping-tables)
- [Package Organization](#package-organization)
- [Design Decisions](#design-decisions)

---

## Architecture Relationship

```
┌─────────────────────────────────────────────────────────────┐
│                     Client Application                       │
│                                                              │
│   with Zoneinfo.API.Desktop;                                │
│   Result := Now_Zoned (Zone_ID);                            │
└────────────────────────┬────────────────────────────────────┘
                         │
┌────────────────────────▼────────────────────────────────────┐
│                       Zoneinfo                               │
│                                                              │
│  API Layer → Application Layer → Domain Layer                │
│       ↓              ↓                                       │
│  Infrastructure.Adapter.Tzif_Adapter                        │
│       ↓                                                      │
│     TZif.API (external crate)                               │
└─────────────────────────────────────────────────────────────┘
```

The `Tzif_Adapter` in Zoneinfo's infrastructure layer implements the `Timezone_Port` interface, delegating to TZif for all timezone data operations.

---

## Type Mapping

### Zoneinfo Types → TZif Types

| Zoneinfo Type | Purpose | TZif Equivalent |
|---------------|---------|-----------------|
| `Zone_ID` | IANA timezone identifier | `TZif.Domain.Value_Object.Zone_Id_Type` |
| `Instant` | Absolute moment (epoch nanos) | Uses `Epoch_Seconds_Type` for lookups |
| `Zoned` | Instant + Zone | Combines `Instant` with `Zone_ID` |
| `Civil` | Calendar components | Result of TZif offset calculations |
| `Duration_Type` | Time span | N/A (Zoneinfo internal) |

### Key Differences

- **Zoneinfo uses nanoseconds** for `Instant`; TZif uses seconds for `Epoch_Seconds`
- **Zoneinfo `Zoned` does not cache offset** - computed on demand via TZif
- **Zone_ID validation** delegates to TZif's `Find_By_Id`

---

## API Mapping Tables

### Clock Operations

| Zoneinfo Operation | Description | TZif Usage |
|--------------------|-------------|------------|
| `Now` | Current time as Instant | None (clock adapter) |
| `Now_UTC` | Current time in UTC zone | Uses `Zone_ID.UTC` |
| `Now_Zoned(Zone)` | Current time in timezone | Combines clock + TZif zone lookup |

### Timezone Query Operations

| Zoneinfo Operation | Description | TZif Operation |
|--------------------|-------------|----------------|
| `Find_Zone(ID)` | Validate zone exists | `TZif.API.Find_By_Id` |
| `Find_My_Zone` | Detect local timezone | `TZif.API.Find_My_Id` |
| `List_Zones` | List all zones | `TZif.API.List_All_Order_By_Id` |
| `Find_Zones_By_Region` | Search by region | `TZif.API.Find_By_Region` |
| `Find_Zones_By_Pattern` | Search by pattern | `TZif.API.Find_By_Pattern` |

### Conversion Operations

| Zoneinfo Operation | Description | TZif Operation |
|--------------------|-------------|----------------|
| `To_Civil(Zoned)` | Zoned → Civil | Uses `TZif.API.Get_Transition_At_Epoch` for offset |
| `To_Zoned(Civil, Zone)` | Civil → Zoned | Uses TZif for DST gap/overlap detection |
| `With_Zone(Zoned, Zone)` | Change timezone | TZif validates new zone |
| `Get_Offset(Zoned)` | Get UTC offset | `TZif.API.Get_Transition_At_Epoch` |

### Source Operations

| Zoneinfo Operation | Description | TZif Operation |
|--------------------|-------------|----------------|
| `Discover_Sources(Paths)` | Scan for tzdata | `TZif.API.Discover_Sources` |
| `Load_Source(Path)` | Load timezone data | `TZif.API.Load_Source` |
| `Validate_Source(Path)` | Validate source | `TZif.API.Validate_Source` |
| `Get_Version` | Get tzdata version | `TZif.API.Get_Version` |

### Arithmetic Operations

| Zoneinfo Operation | Description | TZif Usage | Returns |
|--------------------|-------------|------------|---------|
| `Instant + Duration` | Add duration | None (pure domain) | `Instant_Result.Result` |
| `Instant - Duration` | Subtract duration | None (pure domain) | `Instant_Result.Result` |
| `Instant - Instant` | Calculate duration | None (pure domain) | `Duration_Type` |
| `Duration + Duration` | Add durations | None (pure domain) | `Duration_Type` |
| `Duration - Duration` | Subtract durations | None (pure domain) | `Duration_Type` |
| `-Duration` | Negate duration | None (pure domain) | `Duration_Type` |

**Note:** Instant operators return `Result` types for overflow detection. Duration operators return values directly since overflow is practically impossible with `Integer_64` range (~292 billion years).

---

## Package Organization

### Zoneinfo API Packages (Current Implementation)

| Package | Purpose | TZif Dependency |
|---------|---------|-----------------|
| `Zoneinfo.API` | Type re-exports | None |
| `Zoneinfo.API.Desktop` | Clock + Timezone operations | Yes (composition root) |
| `Zoneinfo.API.Discovery` | Source management + Zone queries | Yes (delegates to TZif) |
| `Zoneinfo.API.Operations` | Pure arithmetic (SPARK-safe) | None |
| `Zoneinfo.API.Format` | Formatting | None (string operations) |
| `Zoneinfo.API.Parse` | Parsing | None (string operations) |

### Infrastructure Layer

```
Infrastructure.Adapter.Tzif_Adapter
    ├── Implements: Timezone_Port
    ├── Uses: TZif.API
    ├── Operations:
    │   ├── Get_UTC_Offset (Instant, Zone) → Offset_Result
    │   ├── To_Civil (Zoned) → Civil
    │   ├── To_Zoned (Civil, Zone) → Zoned_Result
    │   ├── Is_Valid_Zone (Zone_ID) → Boolean
    │   ├── Find_Zone (Zone_ID) → Zone_Result
    │   └── Get_Transition (Instant, Zone) → Transition_Result
    └── SPARK_Mode: Off (I/O operations)
```

---

## Design Decisions

### 1. Wrap, Don't Expose

**Decision:** Zoneinfo wraps TZif with its own types rather than re-exporting TZif types.

**Rationale:**
- Loose coupling - can swap TZif implementation
- Type safety - Zoneinfo types optimized for datetime operations
- API stability - Zoneinfo API stable even if TZif evolves

### 2. On-Demand Offset Calculation

**Decision:** `Zoned` does not cache UTC offset; computed via TZif when needed.

**Rationale:**
- Single source of truth (TZif database)
- Simpler type (no stale cache issues)
- SPARK-friendly pure domain type

### 3. Operators Return Result

**Decision:** Arithmetic operators like `+` and `-` return `Result[T]` instead of plain values.

**Rationale:**
- Functional error handling (no exceptions)
- Overflow detection
- Consistent with railway-oriented design
- Non-idiomatic but aligned with core architecture tenets

### 4. Seconds → Nanoseconds Bridge

**Decision:** Zoneinfo uses nanosecond precision internally; converts to/from TZif's second-based epochs.

**Rationale:**
- Modern datetime needs (sub-second precision)
- TZif transitions are second-granularity (IANA format limitation)
- Bridge logic in Tzif_Adapter handles conversion

---

## Example: Zone Query Flow

```ada
--  User code
Result := Zoneinfo.API.Timezone.Find_Zone ("America/New_York");

--  Internal flow:
--  1. Zoneinfo.API.Timezone.Find_Zone calls
--  2. Application.Usecase.Find_Zone.Execute which calls
--  3. Infrastructure.Adapter.Tzif_Adapter.Find_Zone which calls
--  4. TZif.API.Find_By_Id ("America/New_York")
--  5. TZif returns Zone or Error
--  6. Tzif_Adapter converts TZif.Domain.Entity.Zone to Zoneinfo.Domain.Zone_ID
--  7. Result propagates back up
```

---

## Example: Civil Conversion Flow

```ada
--  User code: Convert Zoned to Civil
Civil_Time := To_Civil (My_Zoned);

--  Internal flow:
--  1. To_Civil extracts Instant and Zone_ID from Zoned
--  2. Calls Tzif_Adapter.Get_UTC_Offset (Instant, Zone_ID)
--  3. Tzif_Adapter calls TZif.API.Get_Transition_At_Epoch
--  4. TZif returns Transition_Info with UTC offset and abbreviation
--  5. Tzif_Adapter applies offset to Instant → calendar components
--  6. Returns Civil value
```

---

## Platform Notes

The Tzif_Adapter delegates all platform-specific operations to TZif:

| Platform | Timezone Source | Local Zone Detection |
|----------|-----------------|----------------------|
| Linux/BSD/macOS | `/usr/share/zoneinfo` | `/etc/localtime` symlink |
| Windows 10+ | User-provided path | Win32 API + CLDR mapping |

See the [TZif repository](https://github.com/abitofhelp/tzif_ada) for platform setup details.

---

## See Also

- [Software Design Specification](../formal/software_design_specification.md) - Architecture details
- [Error Handling Strategy](../common/guides/error_handling_strategy.md) - Result monad usage
- [All About Our API](../common/guides/all_about_our_api.md) - Three-package pattern

---

**Document Control**:
- Version: 1.0.0
- Last Updated: 2025-12-15
- Status: Released
- Copyright © 2025 Michael Gardner, A Bit of Help, Inc.
- License: BSD-3-Clause
