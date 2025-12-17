# Changelog

**Version:** 1.1.1<br>
**Date:** 2025-12-17<br>
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>
**Status:** Released

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.1.1] - 2025-12-17

**Tests:** 335 unit + 154 integration = 489 total - All passing<br>
**SPARK Status:** 710 checks: 60 flow, 596 proved, 54 unproved (~92% proved) (--mode=prove --level=2)<br>

### Changed

- **Dependency** - Updated tzif to ^3.0.3

---

## [1.1.0] - 2025-12-16

**Tests:** 335 unit + 154 integration = 489 total - All passing<br>
**SPARK Status:** 710 checks: 60 flow, 596 proved, 54 unproved (~92% proved) (--mode=prove --level=2)<br>

### Added

- **Bounded array types** for zone listing - `Zone_List` and `Search_Results` with configurable capacity per profile
- **Memory planning constants** - `Max_Zone_List_Size`, `Max_Search_Results`, `Zone_ID_Size_Bytes`, `Zone_List_Memory_Bytes`, `Search_Results_Memory_Bytes` in all config profiles

### Changed

- **Discovery API** - `List_All_Zones` and `Find_By_*` now return `Result[Zone_List/Search_Results]` instead of using callbacks
- **SPARK_Mode** - Enabled `SPARK_Mode => On` for all Domain and Application layer specs
- **Removed Zone_Callback** - Eliminated `access procedure` type for SPARK compatibility
- **Domain.Error.Result** - Slimmed to 7 essential operations for SPARK compatibility
  - Kept: `Ok`, `Error`, `From_Error`, `Is_Ok`, `Is_Error`, `Value`, `Error_Info`
  - Removed combinators available via `Functional.Result` in infrastructure layer
- **Dependency** - Requires tzif ^3.0.0

### Fixed

- **SPARK prover crash** - Eliminated GNAT BUG DETECTED error in Result's Fallback function
- **Windows CI** - Fixed TZIF_DATA_PATH handling with forward slashes for GitHub Actions

---

## [1.0.0] - 2025-12-15

**Tests:** 510 (356 unit + 154 integration) - All passing<br>
**SPARK:** Checked (Domain + Application layers)

Initial release of the Zoneinfo timezone-aware datetime library for Ada 2022.

### Added

- **Domain Value Objects**
  - Instant - Absolute moment in time (epoch nanoseconds)
  - Zoned - Instant with timezone context
  - Civil - Timezone-blind calendar components (year, month, day, hour, minute, second, nanosecond)
  - Duration_Type - Time span with nanosecond precision
  - Zone_ID - IANA timezone identifier (bounded string)
  - Source_Info - Timezone database source metadata

- **ISO 8601 Parsing** (`Zoneinfo.API.Parse`)
  - Parse datetime strings: `From_ISO_8601`
  - Parse with UTC offset: `From_ISO_8601_With_Offset`
  - Parse with zone ID: `From_ISO_8601_With_Zone`
  - Parse full format: `From_ISO_8601_Full`
  - Parse date-only: `From_ISO_Date`
  - Parse time-only: `From_ISO_Time`
  - Parse ISO 8601 durations: `From_ISO_Duration`
  - Parse human-readable durations: `From_Human_Duration`
  - Parse UTC offsets: `Parse_Offset`

- **Datetime Formatting** (`Zoneinfo.API.Format`)
  - Format as ISO 8601: `To_ISO_8601`
  - Format with offset: `To_ISO_8601_With_Offset`
  - Format with timezone: `To_ISO_8601_With_Zone`
  - Format date/time portions: `To_ISO_Date`, `To_ISO_Time`
  - Format durations: `To_ISO_Duration`, `To_Human_Duration`

- **Timezone Discovery** (`Zoneinfo.API.Discovery`)
  - Discover timezone sources: `Discover_Sources`
  - Load timezone source: `Load_Source`
  - Validate timezone source: `Validate_Source`
  - Find system timezone: `Find_My_Id`
  - Get database version: `Get_Version`
  - List all zones: `List_All_Zones`
  - Search by pattern: `Find_By_Pattern`
  - Search by region: `Find_By_Region`
  - Search by regex: `Find_By_Regex`

- **Pure Operations** (`Zoneinfo.API.Operations`)
  - Instant arithmetic: Add, Subtract, Diff
  - Duration arithmetic: Add, Subtract, Negate
  - Operators: `+`, `-` for Result-based arithmetic
  - All operations SPARK-verified

- **Architecture**
  - 4-layer hexagonal architecture (Domain → Application → Infrastructure → API)
  - Dependency inversion via ports and adapters
  - Static dependency injection
  - Result monad error handling (no exceptions)
  - TZif library integration for IANA timezone database access

- **Error Handling**
  - Seven error kinds: Validation, Timezone, Overflow, Ambiguous_Time, Gap_Time, IO, Internal
  - Functional error handling via Result monad (from `functional` crate)
  - Detailed error messages with context

- **Testing**
  - 356 unit tests (Domain, Application, Infrastructure, API layers)
  - 154 integration tests (cross-layer functionality)
  - 510 total tests, all passing
  - Test coverage framework via AUnit

- **Build System**
  - Alire package manager support
  - GPR project files for modular builds
  - Makefile convenience targets
  - Debug and release profiles
  - SPARK verification support

- **Documentation**
  - Software Requirements Specification
  - Software Design Specification
  - Software Test Guide
  - Quick Start Guide
  - Architecture guides
  - Comprehensive API documentation in source files

### Changed

- **Parse helpers** - Refactored to exception-safe implementation using Functional.Option (Preelaborate compatible)

### Fixed

- **Windows CI** - Corrected TZIF_DATA_PATH handling with forward slashes for GitHub Actions
- **Ada 2022 compliance** - Fixed reserved word conflict (`some` → `New_Some`)

### Technical Details

- **Test Coverage**: 510 tests (356 unit + 154 integration), all passing
- **SPARK Status**: Domain + Application layers verified (--mode=check)
- **Dependencies**: functional ^4.0.0, tzif ^3.0.1, gnatcoll ^25.0.0
- **Compiler**: GNAT 14+, Ada 2022
- **Platforms**: Desktop (Linux, macOS, Windows)
- **License**: BSD-3-Clause

### Design Decisions

- **No Exceptions**: All errors returned via Result monad for functional error handling
- **TZif Integration**: Built on TZif library for IANA timezone database access
- **Value Objects**: Immutable domain types with validation
- **SPARK Boundaries**: Domain + Application layers verified, Infrastructure + API layers use I/O
- **Static Dispatch**: Generic instantiation for zero runtime overhead
- **Library Standalone**: Explicit Library_Interface for ABI stability

[1.1.1]: https://github.com/abitofhelp/zoneinfo/releases/tag/v1.1.1
[1.1.0]: https://github.com/abitofhelp/zoneinfo/releases/tag/v1.1.0
[1.0.0]: https://github.com/abitofhelp/zoneinfo/releases/tag/v1.0.0
