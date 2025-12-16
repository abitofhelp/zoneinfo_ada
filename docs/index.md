# Zoneinfo Library Documentation

**Version:** 1.0.0<br>
**Date:** December 15, 2025<br>
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
- SPARK-verified domain and application layers

---

## Quick Navigation

### Getting Started

- **[Quick Start Guide](./quick_start.md)** - Installation, basic usage, and first programs
- **[README](../README.md)** - Project overview and feature list

### Formal Documentation

- **[Software Requirements Specification](./formal/software_requirements_specification.md)** - Complete functional and non-functional requirements
- **[Software Design Specification](./formal/software_design_specification.md)** - Architecture, design patterns, and implementation details
- **[Software Test Guide](./formal/software_test_guide.md)** - Test strategy, execution, and writing new tests

### Developer Guides

- **[Architecture Enforcement](./guides/architecture_enforcement.md)** - Layer dependency rules and DIP compliance
- **[Build Profiles](./guides/build_profiles.md)** - Multi-platform build configuration

### Reference

- **[CHANGELOG](../CHANGELOG.md)** - Release history and version details

---

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                     Zoneinfo.API (Facade)                       │
│           Re-exports Domain Types + Composition Roots            │
├──────────────┬──────────────┬──────────────┬───────────────────┤
│ API.Desktop  │ API.Discovery│  API.Format  │  API.Parse        │
│ (Desktop I/O)│ (TZ Discovery)│ (ISO 8601)   │  (Parse Strings)  │
├──────────────┴──────────────┴──────────────┴───────────────────┤
│                      Application Layer                          │
│     Use Cases │ Commands │ Ports (Clock, Timezone)              │
├─────────────────────────────────────────────────────────────────┤
│                    Infrastructure Layer                         │
│  Adapters: Desktop_Clock │ Tzif │ Discovery                     │
├─────────────────────────────────────────────────────────────────┤
│                       Domain Layer                              │
│  Value Objects: Instant │ Zoned │ Civil │ Duration │ Zone_ID    │
│  Error Handling │ Unit Type │ Result Monad                      │
└─────────────────────────────────────────────────────────────────┘

Dependency Direction: ▲ (Lower layers independent of upper layers)
```

**Design Principles:**

- **Hexagonal Architecture** - Ports & adapters with dependency inversion
- **Clean Architecture** - Domain-centric with infrastructure abstraction
- **Functional Error Handling** - Result monad (no exceptions)
- **SPARK Verification** - Domain + Application layers formally verified
- **Embedded Safety** - No heap allocations, bounded types

---

## API Operations

### Desktop API (Zoneinfo.API.Desktop)

**Clock Operations:**
- `Now` - Get current time as Instant
- `Now_Zoned` - Get current time in a specific timezone
- `Now_UTC` - Get current UTC time

**Timezone Conversions:**
- `To_Civil` - Convert Instant/Zoned to wall clock time
- `To_Zoned` - Convert Civil time to Zoned (may fail with Gap/Ambiguous errors)
- `To_Instant` - Convert Civil + timezone to Instant
- `With_Zone` - Change timezone of a Zoned value

**Utilities:**
- `Get_Offset` - Get UTC offset for a Zoned value
- `Is_Valid_Zone` - Check if Zone_ID exists in TZif database

### Discovery API (Zoneinfo.API.Discovery)

**Source Management:**
- `Discover_Sources` - Scan paths for IANA timezone databases
- `Load_Source` - Load timezone data from a path
- `Validate_Source` - Verify timezone source integrity

**Timezone Queries:**
- `Find_My_Id` - Get local system timezone
- `Get_Version` - Get timezone database version
- `List_All_Zones` - Enumerate all available timezone IDs

**Pattern-Based Search:**
- `Find_By_Pattern` - Substring search (e.g., "York" → "America/New_York")
- `Find_By_Region` - Search by region (e.g., "America")
- `Find_By_Regex` - Regular expression search

### Format API (Zoneinfo.API.Format)

**Civil Formatting:**
- `To_ISO_8601` - "2025-12-15T14:30:00.123456789"
- `To_ISO_8601_With_Offset` - "2025-12-15T14:30:00-05:00"
- `To_ISO_8601_With_Zone` - "2025-12-15T14:30:00[America/New_York]"
- `To_ISO_8601_Full` - "2025-12-15T14:30:00-05:00[America/New_York]"
- `To_ISO_Date` - "2025-12-15"
- `To_ISO_Time` - "14:30:00.123456789"

**Duration Formatting:**
- `To_ISO_Duration` - "PT1H30M45S" (ISO 8601)
- `To_Human_Duration` - "1h 30m 45s" (human-readable)
- `Format_Offset` - "+05:00" or "Z"

**Instant Formatting:**
- `To_Epoch_String` - "1734283800.123456789"

### Parse API (Zoneinfo.API.Parse)

**Civil Parsing:**
- `From_ISO_8601` - Parse "2025-12-15T14:30:00"
- `From_ISO_8601_With_Offset` - Parse "2025-12-15T14:30:00-05:00"
- `From_ISO_8601_With_Zone` - Parse "2025-12-15T14:30:00[America/New_York]"
- `From_ISO_8601_Full` - Parse full ISO 8601 with offset and zone
- `From_ISO_Date` - Parse "2025-12-15"
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
- Duration arithmetic operators

---

## Platform Support

| Platform | Status | Clock Source | Timezone Discovery |
|----------|--------|--------------|-------------------|
| **Linux** | ✅ Full | `Ada.Calendar.Clock` | `/etc/localtime`, `/usr/share/zoneinfo` |
| **macOS** | ✅ Full | `Ada.Calendar.Clock` | `/etc/localtime`, `/usr/share/zoneinfo` |
| **Windows** | ✅ Full | `Ada.Calendar.Clock` | Registry + Windows API |
| **Embedded** | 🔧 Custom | User-provided clock port | User-provided TZif source |

**For embedded platforms**, implement the clock port and provide a TZif data source. See [Software Design Specification](./formal/software_design_specification.md) §3.3 for details.

---

## Documentation Tree

```
docs/
├── index.md                              # This file - main documentation hub
├── quick_start.md                        # Get started in minutes
│
├── formal/                               # Formal specifications
│   ├── software_requirements_specification.md
│   ├── software_design_specification.md
│   └── software_test_guide.md
│
├── guides/                               # Developer guides
│   ├── architecture_enforcement.md       # Layer dependency rules
│   └── build_profiles.md                 # Multi-platform configuration
│
└── diagrams/                             # UML diagrams
    ├── domain_types.puml / .svg          # Three datetime kinds
    └── clock_port.puml / .svg            # Pluggable time source pattern
```

---

## Need Help?

- **Getting started?** → [Quick Start Guide](./quick_start.md)
- **Running tests?** → [Software Test Guide](./formal/software_test_guide.md)
- **Understanding errors?** → [Quick Start §8: Error Handling](./quick_start.md#error-handling)
- **Architecture questions?** → [Software Design Specification](./formal/software_design_specification.md)

---

**License:** BSD-3-Clause
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.
