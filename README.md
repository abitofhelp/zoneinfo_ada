# Zoneinfo - Timezone-Aware Datetime Library for Ada 2022

[![License](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE) [![Ada](https://img.shields.io/badge/Ada-2022-blue.svg)](https://ada-lang.io) [![SPARK](https://img.shields.io/badge/SPARK-Checked-yellow.svg)](https://www.adacore.com/about-spark) [![Alire](https://img.shields.io/badge/Alire-2.0+-blue.svg)](https://alire.ada.dev)

**Version:** 1.0.0<br>
**Date:** 2025-12-15<br>
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>
**Status:** Released

## Overview

Zoneinfo is a timezone-aware datetime manipulation library for Ada 2022. Built on the TZif library, it provides a clean, type-safe API for working with timezones, parsing ISO 8601 strings, formatting datetimes, and discovering timezone sources. The library follows **hybrid DDD/Clean/Hexagonal architecture** with functional error handling and is designed for both desktop and embedded platforms.

## Features

- ✅ **Timezone-Aware Datetimes** - Instant, Zoned, and Civil time representations
- ✅ **ISO 8601 Parsing** - Parse datetime strings with timezone offsets and zone IDs
- ✅ **Datetime Formatting** - Format datetimes as ISO 8601 or custom patterns
- ✅ **Timezone Discovery** - Find system timezone, search by pattern/region/regex
- ✅ **Duration Arithmetic** - Add/subtract durations, calculate differences
- ✅ **TZif Integration** - Built on the TZif library for IANA timezone database access
- ✅ **Result Monad Error Handling** - No exceptions, functional error handling
- ✅ **4-Layer Hexagonal Architecture** - Domain → Application → Infrastructure → API
- ✅ **Embedded Safety** - No implicit heap allocations, static dispatch
- ✅ **Library Standalone** - Explicit Library_Interface for ABI stability

## SPARK Formal Verification

<table>
<tr>
<td width="120"><strong>Status</strong></td>
<td><img src="https://img.shields.io/badge/SPARK-Checked-yellow.svg" alt="SPARK Checked"></td>
</tr>
<tr>
<td><strong>Scope</strong></td>
<td>Domain + Application Layers</td>
</tr>
<tr>
<td><strong>Mode</strong></td>
<td>gnatprove --mode=check (SPARK legality verified)</td>
</tr>
<tr>
<td><strong>Results</strong></td>
<td>See <a href="CHANGELOG.md">CHANGELOG</a> for current verification statistics</td>
</tr>
</table>

The **domain and application layers** are formally verified using SPARK Ada legality checks, ensuring:

- **No uninitialized data** - All variables properly initialized before use
- **Data flow integrity** - No aliasing or information flow violations
- **Contract consistency** - Pre/postconditions are consistent

Infrastructure and API layers use `SPARK_Mode => Off` as they perform I/O operations and TZif integration.

### Verification Commands

```bash
make spark-check    # Run SPARK legality verification
make spark-prove    # Run full SPARK proof verification
```

### SPARK Coverage

| Layer | SPARK_Mode | Description |
|-------|-----------|-------------|
| Domain | On | Value objects (Instant, Zoned, Civil, Duration, Zone_ID) |
| Application | On | Use cases and ports |
| Infrastructure | Off | I/O operations, TZif adapters |
| API | Off | Facade over infrastructure |

## Getting Started

### Clone with Submodules

This repository uses git submodules for shared tooling. Clone with:

```bash
git clone --recurse-submodules https://github.com/abitofhelp/zoneinfo.git
```

Or if already cloned without submodules:

```bash
git submodule update --init --recursive
# Or: make submodule-init
```

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                      Zoneinfo.API                           │
│              (Public Facade - Stable Interface)             │
├─────────────────────────────────────────────────────────────┤
│  API.Operations  │  API.Parse  │  API.Format  │ API.Discovery│
│  (Pure ops)      │  (ISO 8601) │  (Display)   │ (TZ sources) │
├─────────────────────────────────────────────────────────────┤
│                    Application Layer                        │
│         Use Cases  │  Ports (Timezone)  │  Commands         │
├─────────────────────────────────────────────────────────────┤
│                   Infrastructure Layer                      │
│         Adapters (TZif Integration, Discovery)              │
├─────────────────────────────────────────────────────────────┤
│                      Domain Layer                           │
│  Instant │ Zoned │ Civil │ Duration │ Zone_ID │ Source_Info │
└─────────────────────────────────────────────────────────────┘
```

## Quick Start

### Building

```bash
# Build debug library
make build

# Build release library
make build-release

# Using Alire directly
alr build
```

### Using in Your Project

Add to your `alire.toml`:

```toml
[[depends-on]]
zoneinfo = "^1.0.0"
```

In your Ada code:

```ada
with Zoneinfo.API;
with Zoneinfo.API.Parse;
with Zoneinfo.API.Format;
use Zoneinfo.API;

procedure Main is
   --  Parse ISO 8601 string
   Parse_Result : constant Civil_Result.Result :=
     Parse.From_ISO_8601 ("2025-12-15T14:30:00");

   --  Create a timezone
   Zone_Result : constant Zone_ID_Result.Result :=
     Zone_ID_Pkg.From_String ("America/New_York");

   --  Create duration
   One_Hour : constant Duration_Type :=
     Duration_Pkg.From_Hours (1);

   --  Format datetime
   Formatted : constant String :=
     Format.To_ISO_8601 (Some_Civil_Time);
begin
   if Civil_Result.Is_Ok (Parse_Result) then
      --  Success! Use the parsed Civil time
      null;
   else
      --  Handle error
      null;
   end if;
end Main;
```

## Usage Examples

### Working with Timezones

```ada
with Zoneinfo.API;
use Zoneinfo.API;

--  UTC is a convenience constant (no Result unwrapping needed)
Zone : constant Zone_ID := UTC;

--  Create instant (epoch nanoseconds)
Now : constant Instant := Instant_Pkg.From_Epoch_Nanos (1700000000);

--  Create zoned datetime
Zoned_Time : constant Zoned := Zoned_Pkg.Create (Now, Zone);

--  Change timezone (preserves instant)
--  Note: From_String returns Result, so handle errors in production code
NY_Result : constant Zone_ID_Result.Result :=
  Zone_ID_Pkg.From_String ("America/New_York");
NY_Zone : Zone_ID;

if Zone_ID_Result.Is_Ok (NY_Result) then
   NY_Zone := Zone_ID_Result.Value (NY_Result);
   NY_Time := Zoned_Pkg.With_Zone (Zoned_Time, NY_Zone);
end if;
```

### Parsing ISO 8601

```ada
with Zoneinfo.API.Parse;

--  Parse datetime
Result := Parse.From_ISO_8601 ("2025-12-15T14:30:00");

--  Parse with offset
Result := Parse.From_ISO_8601_With_Offset ("2025-12-15T14:30:00-05:00");

--  Parse with zone
Result := Parse.From_ISO_8601_With_Zone ("2025-12-15T14:30:00[America/New_York]");

--  Parse duration
Duration_Result := Parse.From_ISO_Duration ("PT1H30M");
```

### Formatting

```ada
with Zoneinfo.API.Format;

--  Format returns Datetime_String (bounded); use To_String to convert
ISO_Bounded : constant Format.Datetime_String :=
  Format.To_ISO_8601 (Civil_Time);
ISO_String  : constant String := Format.To_String (ISO_Bounded);

--  Format date portion only
Date_Only : constant Format.Datetime_String := Format.To_ISO_Date (Civil_Time);

--  Format with offset
With_Offset : constant Format.Datetime_String :=
  Format.To_ISO_8601_With_Offset (Civil_Time, UTC_Offset);
```

### Timezone Discovery

```ada
with Zoneinfo.API.Discovery;

--  Find system timezone
My_Zone : constant Zone_ID_Result.Result := Discovery.Find_My_Id;

--  Discover timezone sources
Paths : Path_List (1 .. 1);
Paths (1) := Make_Path ("/usr/share/zoneinfo");
Source_Result : constant Source_Info_Result.Result :=
  Discovery.Discover_Sources (Paths);

--  Search by pattern
Result := Discovery.Find_By_Pattern ("York", Yield_Callback);
```

## Testing

```bash
# Run all tests
make test-all

# Build tests
make build-tests

# Run unit tests only
./test/bin/unit_runner

# Run integration tests only
./test/bin/integration_runner
```

**Test Results**: All 510 tests passing (356 unit + 154 integration)

## Documentation

- 📚 **[Documentation Index](docs/index.md)** - Complete documentation overview
- 🚀 **[Quick Start Guide](docs/quick_start.md)** - Get started in minutes
- 📖 **[Software Requirements Specification](docs/formal/software_requirements_specification.md)**
- 🏗️ **[Software Design Specification](docs/formal/software_design_specification.md)**
- 🧪 **[Software Test Guide](docs/formal/software_test_guide.md)**
- 📝 **[CHANGELOG](CHANGELOG.md)** - Release history

## Code Standards

This project follows:
- **Ada Agent** (`~/.claude/agents/ada.md`) - Ada 2022 standards
- **Architecture Agent** (`~/.claude/agents/architecture.md`) - DDD/Clean/Hexagonal
- **Functional Agent** (`~/.claude/agents/functional.md`) - Result/Option patterns
- **SPARK Agent** (`~/.claude/agents/spark.md`) - Formal verification patterns

## Submodule Management

This project uses git submodules for shared Python tooling:

- `scripts/python` - Build, release, and architecture scripts
- `test/python` - Shared test fixtures and configuration

### Commands

```bash
# After fresh clone
make submodule-init

# Pull latest from submodule repos
make submodule-update

# Check current submodule commits
make submodule-status
```

## Contributing

This project is not open to external contributions at this time.

## AI Assistance & Authorship

This project — including its source code, tests, documentation, and other deliverables — is designed, implemented, and maintained by human developers, with Michael Gardner as the Principal Software Engineer and project lead.

We use AI coding assistants (such as OpenAI GPT models and Anthropic Claude Code) as part of the development workflow to help with:

- drafting and refactoring code and tests,
- exploring design and implementation alternatives,
- generating or refining documentation and examples,
- and performing tedious and error-prone chores.

AI systems are treated as tools, not authors. All changes are reviewed, adapted, and integrated by the human maintainers, who remain fully responsible for the architecture, correctness, and licensing of this project.

## License

Copyright © 2025 Michael Gardner, A Bit of Help, Inc.

Licensed under the BSD-3-Clause License. See [LICENSE](LICENSE) for details.

## Author

Michael Gardner<br>
A Bit of Help, Inc.<br>
https://github.com/abitofhelp

## Project Status

**Status**: Released (v1.0.0)

- ✅ Core timezone-aware datetime types (Instant, Zoned, Civil)
- ✅ ISO 8601 parsing and formatting
- ✅ Timezone discovery and search operations
- ✅ Duration arithmetic and comparisons
- ✅ TZif library integration
- ✅ 4-layer hexagonal architecture
- ✅ Full test suite (510 tests)
- ✅ Comprehensive documentation
- ✅ SPARK legality verification for Domain + Application layers
- ✅ Alire publication
