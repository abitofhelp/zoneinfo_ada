# ZoneInfo - IANA Timezone Operations Library for Ada 2022

[![License](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE)
[![Ada](https://img.shields.io/badge/Ada-2022-blue.svg)](https://ada-lang.io)
[![Alire](https://img.shields.io/badge/Alire-2.0-blue.svg)](https://alire.ada.dev)

> ⚠️ **IMPORTANT**: Version 1.0.0-rc2 is a **non-functional stub** for Alire name reservation only. All API operations return errors. Do not use this version. Wait for v1.0.0 which will be released after the TZif dependency is approved in Alire.

High-level Ada 2022 library for timezone operations using the IANA timezone database.

## Features

- 🕐 **UTC Offset Queries**: Get UTC offset for any timezone at any time
- 🔄 **Time Conversion**: Convert times between any two timezones
- 🌅 **DST Disambiguation**: Handle ambiguous times during DST transitions
  - Configurable strategies: Prefer_Earlier, Prefer_Later, Raise_On_Ambiguous
  - Detect ambiguous times (fall-back) and gaps (spring-forward)
  - Get detailed ambiguity information with all possible offsets
- 🔍 **Timezone Discovery**: Find local timezone, search by pattern or region
- 📋 **Timezone Enumeration**: List all available IANA timezones
- ℹ️ **Timezone Information**: Get abbreviations, check DST status
- 🧱 **Railway-Oriented Programming**: Result monads, no exceptions
- 🏗️ **Hexagonal Architecture**: Clean separation of concerns
- 🔒 **Thread-Safe**: Safe concurrent operations
- 🌍 **Cross-Platform**: Linux, macOS, BSD

## Current Status: v1.0.0-rc2 (Stub Release)

**This release is for Alire name reservation only.**

- ⚠️ All API operations return `Infrastructure_Error`
- ⚠️ TZif dependency is commented out (pending Alire approval)
- ⚠️ Do not use this version for development

**Full functionality will be available in v1.0.0** after TZif is approved in Alire.

## Quick Start (Post v1.0.0)

### Installation

```bash
# Using Alire (after v1.0.0 release)
alr get zoneinfo
cd zoneinfo_*
alr build
```

### Basic Usage

```ada
with ZoneInfo.API;
with Domain.Value_Object.Zone_Id.Result;
with Ada.Calendar;
with Ada.Text_IO; use Ada.Text_IO;

procedure My_App is
   Zone_Result : constant ZoneInfo.Zone_Id_Result :=
     Domain.Value_Object.Zone_Id.Result.Create ("America/New_York");
begin
   if Domain.Value_Object.Zone_Id.Result.Is_Ok (Zone_Result) then
      declare
         NYC : constant ZoneInfo.Zone_Id :=
           Domain.Value_Object.Zone_Id.Result.Value (Zone_Result);

         Offset_Result : constant ZoneInfo.UTC_Offset_Result :=
           ZoneInfo.API.Get_UTC_Offset (NYC, Ada.Calendar.Clock);
      begin
         if Domain.Value_Object.UTC_Offset.Result.Is_Ok (Offset_Result) then
            Put_Line ("Current NYC offset: " &
                     Domain.Value_Object.UTC_Offset.To_String (
                       Domain.Value_Object.UTC_Offset.Result.Value (Offset_Result)));
         end if;
      end;
   end if;
end My_App;
```

## API Overview

### Core Timezone Operations

- `Get_UTC_Offset` - Query UTC offset for any time in any timezone
- `Get_UTC_Offset_With_Disambiguation` - Handle ambiguous DST times with strategy
- `Convert_Time` - Convert times between timezones
- `Get_Ambiguity_Info` - Get detailed DST transition information
- `Is_Ambiguous_Time` - Check if time occurs twice (DST fall-back)
- `Is_Gap_Time` - Check if time doesn't exist (DST spring-forward)

### Timezone Discovery

- `Find_Local_Timezone` - Detect system timezone
- `List_All_Timezones` - Enumerate all IANA timezones
- `Get_Timezone_Count` - Count available timezones
- `Find_Timezones_By_Pattern` - Search by substring pattern
- `Find_Timezones_By_Region` - Filter by geographic region

### Timezone Information

- `Get_Timezone_Abbreviation` - Get abbreviation (EST, PDT, GMT, etc.)
- `Is_DST_Active` - Check if DST is currently active

## Documentation

- [CHANGELOG](CHANGELOG.md)
- [Software Requirements Specification](docs/software_requirements_specification.md)
- [Software Design Specification](docs/software_design_specification.md)
- [Software Test Guide](docs/software_test_guide.md)

## Examples

See `examples/` directory for working examples:
- `comprehensive_test.adb` - Complete API demonstration
- `dst_disambiguation.adb` - DST ambiguity handling
- `timezone_conversion.adb` - Practical conversion scenarios
- `search_timezones.adb` - Timezone search and discovery
- And more...

## Testing (v1.0.0-rc1)

```bash
# Run all validation tests
make test-all
```

**Test Results**: 532 tests passing
- 516 comprehensive validation tests (ZoneInfo vs Python reference)
- 10 DST disambiguation tests
- 6 API completeness tests

## Architecture

ZoneInfo uses **Hexagonal Architecture** (Ports and Adapters):

```
┌─────────────────────────────────┐
│     Application Layer           │
│   (Use Cases, Ports)            │
│    ZoneInfo.API                 │
│  ┌───────────────────────────┐  │
│  │   Domain Layer            │  │
│  │  (Value Objects, Logic)   │  │
│  └───────────────────────────┘  │
└─────────────────────────────────┘
         ↑
         │
  ┌──────┴──────────┐
  │Infrastructure   │
  │  (TZif Adapter) │
  └─────────────────┘
```

**Layers:**
- **Domain**: Value objects (Zone_Id, UTC_Offset, DST_Info), time conversion logic
- **Application**: ZoneInfo.API with all timezone operations
- **Infrastructure**: TZif integration for IANA database access

## Requirements

- **Compiler**: GNAT FSF 15.2+ or GNAT Pro 25.0+
- **Ada Version**: Ada 2022
- **Build System**: Alire 2.0+
- **Dependencies**:
  - `tzif ^1.0.0` (currently pending Alire approval)

## Restoration Instructions (For Developers)

To restore full functionality after TZif is approved:

1. Restore `src/api/zoneinfo-api.adb` from `.backup` file
2. Uncomment TZif dependency in `alire.toml`
3. Uncomment `with "tzif.gpr"` in `config/zoneinfo_config.gpr`
4. Update version to `1.0.0`
5. Rebuild and test

## Contributing

Contributions welcome! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

## License

Copyright © 2025 Michael Gardner, A Bit of Help, Inc.

Licensed under the BSD-3-Clause License. See [LICENSE](LICENSE) for details.

## Project Status

**Current Release**: v1.0.0-rc2 (Stub - Name Reservation Only)

- ⚠️ Non-functional stub implementation
- ⚠️ TZif dependency pending Alire approval
- ⏳ Awaiting TZif approval for v1.0.0 release

**Previous Release**: v1.0.0-rc1 (Functional)

- ✅ All 13 API operations implemented
- ✅ 532 tests passing (100% validation)
- ✅ Zero compiler warnings
- ✅ Full DST disambiguation support
- ✅ Ready for production (pending TZif availability)

## Support

For issues, questions, or contributions:
- 📧 Email: mike@abitofhelp.com
- 🐛 Issues: [GitHub Issues](https://github.com/abitofhelp/zoneinfo/issues)
- 📖 Docs: See `docs/` directory
