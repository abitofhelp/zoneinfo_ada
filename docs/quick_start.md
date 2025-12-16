# Zoneinfo Quick Start Guide

**Version:** 1.0.0<br>
**Date:** 2025-12-15<br>
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>
**Status:** Released

---

## Table of Contents

- [Installation](#installation)
- [First Program](#first-program)
- [Working with Timezones](#working-with-timezones)
- [Parsing ISO 8601](#parsing-iso-8601)
- [Formatting Datetimes](#formatting-datetimes)
- [Duration Arithmetic](#duration-arithmetic)
- [Timezone Discovery](#timezone-discovery)
- [Error Handling](#error-handling)
- [Running Tests](#running-tests)
- [Build Profiles](#build-profiles)
- [Common Issues](#common-issues)
- [Next Steps](#next-steps)

---

## Installation

### Using Alire (Recommended)

```bash
# Add to your project
alr with zoneinfo

# Or get standalone
alr get zoneinfo
cd zoneinfo_*
alr build
```

### Manual Installation

```bash
git clone --recurse-submodules https://github.com/abitofhelp/zoneinfo.git
cd zoneinfo
alr build
```

### Prerequisites

- **Alire** 2.0+ (Ada package manager)
- **GNAT** 14+ (via Alire toolchain)
- **Make** (for convenience targets)
- **TZif Library** 3.0+ (automatically fetched by Alire)
- **Functional Library** 4.0+ (automatically fetched by Alire)
- **IANA Timezone Database** (typically in `/usr/share/zoneinfo` on Unix systems)

---

## First Program

Create a simple program to parse and display a datetime:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Zoneinfo.API;
with Zoneinfo.API.Parse;
with Zoneinfo.API.Format;

procedure Hello_Zoneinfo is
   use Zoneinfo.API;
   use Zoneinfo.API.Parse;
   use Zoneinfo.API.Format;

   --  Parse an ISO 8601 string
   Parse_Result : constant Civil_Result.Result :=
     From_ISO_8601 ("2025-12-15T14:30:00");

   --  Create a timezone
   Zone : constant Zone_ID := Zone_ID_Pkg.From_String ("America/New_York");

   --  Create a duration
   One_Hour : constant Duration_Type := Duration_Pkg.From_Seconds (3600);
begin
   if Civil_Result.Is_Ok (Parse_Result) then
      Put_Line ("Successfully parsed datetime!");

      declare
         Civil_Time : constant Civil := Civil_Result.Value (Parse_Result);
         Formatted  : constant String := To_String (To_ISO_8601 (Civil_Time));
      begin
         Put_Line ("Formatted: " & Formatted);
      end;
   else
      Put_Line ("Parse failed");
   end if;
end Hello_Zoneinfo;
```

**Expected Output:**
```
Successfully parsed datetime!
Formatted: 2025-12-15T14:30:00
```

---

## Working with Timezones

The library provides three core datetime types:

| Type | Description | Use When |
|------|-------------|----------|
| **Instant** | Absolute point in time (epoch nanoseconds) | Working with absolute time, no timezone needed |
| **Zoned** | Instant + timezone context | Need timezone-aware operations |
| **Civil** | Calendar components (Y/M/D/H/M/S/Nanos) | Display or user input |

### Creating Timezones

```ada
with Zoneinfo.API;
use Zoneinfo.API;

--  Create from IANA zone name
UTC_Zone : constant Zone_ID := Zone_ID_Pkg.UTC;
NY_Zone  : constant Zone_ID := Zone_ID_Pkg.From_String ("America/New_York");
LA_Zone  : constant Zone_ID := Zone_ID_Pkg.From_String ("America/Los_Angeles");

--  Convert to string
Zone_Name : constant String := Zone_ID_Pkg.To_String (NY_Zone);
--  Result: "America/New_York"
```

### Creating Instants

```ada
--  From Unix epoch seconds and nanoseconds
Epoch_Result : constant Instant_Result.Result :=
  Instant_Pkg.From_Unix_Epoch (Seconds => 1700000000, Nanos => 500_000_000);

--  From epoch nanoseconds (always succeeds)
Now : constant Instant := Instant_Pkg.From_Epoch_Nanos (1700000000);

--  Unix epoch (1970-01-01 00:00:00 UTC)
Epoch : constant Instant := Instant_Pkg.Epoch;
```

### Creating Zoned Datetimes

```ada
--  Combine instant with timezone
Instant_Val : constant Instant := Instant_Pkg.From_Epoch_Nanos (1700000000);
Zone        : constant Zone_ID := Zone_ID_Pkg.From_String ("America/New_York");
Zoned_Time  : constant Zoned := Zoned_Pkg.Create (Instant_Val, Zone);

--  Extract components
Extracted_Instant : constant Instant := Zoned_Pkg.To_Instant (Zoned_Time);
Extracted_Zone    : constant Zone_ID := Zoned_Pkg.Get_Zone (Zoned_Time);

--  Change timezone (preserves instant, changes zone)
London_Time : constant Zoned := Zoned_Pkg.With_Zone
  (Zoned_Time, Zone_ID_Pkg.From_String ("Europe/London"));
```

### Creating Civil Times

```ada
--  From components
Civil_Time : constant Civil := Civil_Pkg.Create
  (Year        => 2025,
   Month       => 12,
   Day         => 15,
   Hour        => 14,
   Minute      => 30,
   Second      => 0,
   Nanosecond  => 0);

--  Extract components
Year   : constant Integer_64 := Civil_Pkg.Get_Year (Civil_Time);
Month  : constant Integer_64 := Civil_Pkg.Get_Month (Civil_Time);
Day    : constant Integer_64 := Civil_Pkg.Get_Day (Civil_Time);
Hour   : constant Integer_64 := Civil_Pkg.Get_Hour (Civil_Time);
Minute : constant Integer_64 := Civil_Pkg.Get_Minute (Civil_Time);
Second : constant Integer_64 := Civil_Pkg.Get_Second (Civil_Time);
Nanos  : constant Nanoseconds_Range := Civil_Pkg.Get_Nanosecond (Civil_Time);
```

---

## Parsing ISO 8601

The `Zoneinfo.API.Parse` package provides comprehensive ISO 8601 parsing:

### Parsing Datetimes

```ada
with Zoneinfo.API.Parse;

--  Basic datetime (no timezone)
Result := Parse.From_ISO_8601 ("2025-12-15T14:30:00");
Result := Parse.From_ISO_8601 ("2025-12-15T14:30:00.123456789");

--  With UTC offset
Result := Parse.From_ISO_8601_With_Offset ("2025-12-15T14:30:00-05:00");
Result := Parse.From_ISO_8601_With_Offset ("2025-12-15T14:30:00Z");

--  With zone ID
Result := Parse.From_ISO_8601_With_Zone ("2025-12-15T14:30:00[America/New_York]");

--  With offset AND zone
Result := Parse.From_ISO_8601_Full ("2025-12-15T14:30:00-05:00[America/New_York]");
```

### Parsing Dates and Times

```ada
--  Date only (time defaults to 00:00:00)
Date_Result := Parse.From_ISO_Date ("2025-12-15");

--  Time only (date defaults to 1970-01-01)
Time_Result := Parse.From_ISO_Time ("14:30:00");
Time_Result := Parse.From_ISO_Time ("14:30:00.123456789");
```

### Parsing Durations

```ada
--  ISO 8601 duration format: PnDTnHnMnS
Duration_Result := Parse.From_ISO_Duration ("PT1H30M");      --  1.5 hours
Duration_Result := Parse.From_ISO_Duration ("P1DT12H");      --  1 day 12 hours
Duration_Result := Parse.From_ISO_Duration ("-PT5M");        --  -5 minutes
Duration_Result := Parse.From_ISO_Duration ("PT0S");         --  Zero

--  Human-readable format
Duration_Result := Parse.From_Human_Duration ("1h 30m 45s");
Duration_Result := Parse.From_Human_Duration ("2d 12h");
Duration_Result := Parse.From_Human_Duration ("-5m 30s");
```

### Error Handling Example

```ada
with Zoneinfo.API.Parse;

Parse_Result : constant Civil_Result.Result :=
  Parse.From_ISO_8601 ("2025-13-45T99:99:99");  --  Invalid!

if Civil_Result.Is_Ok (Parse_Result) then
   --  Success
   Civil_Time : constant Civil := Civil_Result.Value (Parse_Result);
else
   --  Error
   Err : constant Error_Type := Civil_Result.Error_Info (Parse_Result);
   Put_Line ("Parse failed: " & Error_Strings.To_String (Err.Message));
end if;
```

---

## Formatting Datetimes

The `Zoneinfo.API.Format` package provides ISO 8601 formatting:

### Formatting Civil Times

```ada
with Zoneinfo.API.Format;
use Zoneinfo.API.Format;

Civil_Time : constant Civil := ...;

--  Basic ISO 8601 (no timezone)
S := To_String (To_ISO_8601 (Civil_Time));
--  Result: "2025-12-15T14:30:00" or "2025-12-15T14:30:00.123456789"

--  Without nanoseconds
S := To_String (To_ISO_8601 (Civil_Time, Include_Nanos => False));
--  Result: "2025-12-15T14:30:00"
```

### Formatting with Timezones

```ada
Offset : constant Duration_Type := Duration_Pkg.From_Seconds (-18000);  --  -5 hours
Zone   : constant Zone_ID := Zone_ID_Pkg.From_String ("America/New_York");

--  With UTC offset
S := To_String (To_ISO_8601_With_Offset (Civil_Time, Offset));
--  Result: "2025-12-15T14:30:00-05:00"

--  With zone ID
S := To_String (To_ISO_8601_With_Zone (Civil_Time, Zone));
--  Result: "2025-12-15T14:30:00[America/New_York]"

--  With both offset and zone
S := To_String (To_ISO_8601_Full (Civil_Time, Offset, Zone));
--  Result: "2025-12-15T14:30:00-05:00[America/New_York]"
```

### Formatting Dates and Times

```ada
--  Date only
S := To_String (To_ISO_Date (Civil_Time));
--  Result: "2025-12-15"

--  Time only
S := To_String (To_ISO_Time (Civil_Time));
--  Result: "14:30:00" or "14:30:00.123456789"
```

### Formatting Durations

```ada
D : constant Duration_Type := Duration_Pkg.From_Seconds (5445);  --  1h 30m 45s

--  ISO 8601 format
S := To_String (To_ISO_Duration (D));
--  Result: "PT1H30M45S"

--  Human-readable format
S := To_String (To_Human_Duration (D));
--  Result: "1h 30m 45s"
```

### Formatting Instants

```ada
I : constant Instant := Instant_Pkg.From_Epoch_Nanos (1700000000);

--  As epoch seconds
S := To_String (To_Epoch_String (I));
--  Result: "1700000000" or "1700000000.123456789"
```

---

## Duration Arithmetic

Durations support addition, subtraction, and negation:

### Creating Durations

```ada
--  From various units
One_Hour   : constant Duration_Type := Duration_Pkg.From_Seconds (3600);
One_Minute : constant Duration_Type := Duration_Pkg.From_Seconds (60);
One_Milli  : constant Duration_Type := Duration_Pkg.From_Millis (1);
One_Nano   : constant Duration_Type := Duration_Pkg.From_Nanos (1);

--  From components
D : constant Duration_Type := Duration_Pkg.Create
  (Seconds => 90, Nanoseconds => 500_000_000);  --  1.5 minutes
```

### Duration Operations

```ada
D1 : constant Duration_Type := Duration_Pkg.From_Seconds (3600);   --  1 hour
D2 : constant Duration_Type := Duration_Pkg.From_Seconds (1800);   --  30 minutes

--  Addition (using operators)
Sum : constant Duration_Type := D1 + D2;  --  1.5 hours

--  Subtraction (using operators)
Diff : constant Duration_Type := D1 - D2;  --  30 minutes

--  Negation (using operator)
Neg : constant Duration_Type := -D1;  --  -1 hour

--  Named function versions
Sum  : constant Duration_Type := Duration_Pkg.Add (D1, D2);
Diff : constant Duration_Type := Duration_Pkg.Subtract (D1, D2);
Neg  : constant Duration_Type := Duration_Pkg.Negate (D1);
```

### Instant Arithmetic

```ada
Now : constant Instant := Instant_Pkg.From_Epoch_Nanos (1700000000);
One_Hour : constant Duration_Type := Duration_Pkg.From_Seconds (3600);

--  Add duration to instant (returns Result[Instant])
Later_Result : constant Instant_Result.Result := Now + One_Hour;

if Instant_Result.Is_Ok (Later_Result) then
   Later : constant Instant := Instant_Result.Value (Later_Result);
   --  Use Later...
end if;

--  Subtract duration from instant
Earlier_Result : constant Instant_Result.Result := Now - One_Hour;

--  Calculate difference between instants (returns Duration directly)
End_Time   : constant Instant := ...;
Start_Time : constant Instant := ...;
Elapsed    : constant Duration_Type := End_Time - Start_Time;
```

### Duration Queries

```ada
D : constant Duration_Type := Duration_Pkg.From_Seconds (-30);

--  Check if negative
Is_Neg : constant Boolean := Duration_Pkg.Is_Negative (D);  --  True

--  Check if zero
Is_Zero : constant Boolean := Duration_Pkg.Is_Zero (D);     --  False

--  Convert to units
Total_Seconds : constant Integer_64 := Duration_Pkg.To_Seconds (D);      --  -30
Total_Millis  : constant Integer_64 := Duration_Pkg.To_Millis (D);       --  -30000
Total_Nanos   : constant Integer_64 := Duration_Pkg.To_Nanos (D);        --  -30000000000
```

---

## Timezone Discovery

The `Zoneinfo.API.Discovery` package provides timezone source discovery and querying:

### Discovering Timezone Sources

```ada
with Zoneinfo.API.Discovery;
use Zoneinfo.API.Discovery;

--  Define search paths
Paths : Path_List (1 .. 2);
Paths (1) := Make_Path ("/usr/share/zoneinfo");
Paths (2) := Make_Path ("/var/db/timezone/zoneinfo");

--  Discover sources
Source_Result : constant Source_Info_Result.Result :=
  Discover_Sources (Paths);

if Is_Ok (Source_Result) then
   Source : constant Source_Info := Value (Source_Result);
   --  Use source...
end if;
```

### Loading and Validating Sources

```ada
--  Load a specific source
Load_Result : constant Source_Info_Result.Result :=
  Load_Source (Make_Path ("/usr/share/zoneinfo"));

--  Validate a source path
Validate_Result : constant Unit_Result.Result :=
  Validate_Source (Make_Path ("/usr/share/zoneinfo"));

if Unit_Result.Is_Ok (Validate_Result) then
   Put_Line ("Source is valid");
end if;
```

### Finding System Timezone

```ada
--  Get the local system timezone
My_Zone_Result : constant Zone_ID_Result.Result := Find_My_Id;

if Is_Ok (My_Zone_Result) then
   My_Zone : constant Zone_ID := Value (My_Zone_Result);
   Put_Line ("System timezone: " & To_String (My_Zone));
end if;
```

### Searching for Timezones

```ada
--  Callback for processing zone IDs
procedure Process_Zone (Id : Zone_ID; Continue : out Boolean) is
begin
   Put_Line ("Found: " & To_String (Id));
   Continue := True;  --  Continue searching
end Process_Zone;

--  Search by pattern (substring match)
Result := Find_By_Pattern ("York", Process_Zone'Access);
--  Finds: America/New_York, etc.

--  Search by region (first component of zone ID)
Result := Find_By_Region ("America", Process_Zone'Access);
--  Finds: America/New_York, America/Chicago, America/Los_Angeles, etc.

--  Search by regex
Result := Find_By_Regex ("America/.*York", Process_Zone'Access);
--  Finds: America/New_York
```

### Listing All Zones

```ada
Source : constant Source_Info := ...;

--  List all zones in ascending order
Result := List_All_Zones
  (Source     => Source,
   Yield      => Process_Zone'Access,
   Descending => False);

--  List in descending order
Result := List_All_Zones
  (Source     => Source,
   Yield      => Process_Zone'Access,
   Descending => True);
```

---

## Error Handling

Zoneinfo uses the **Result monad** pattern for error handling. No exceptions are raised.

### Pattern 1: Check Success/Failure

```ada
Result : constant Civil_Result.Result := Parse.From_ISO_8601 ("...");

if Civil_Result.Is_Ok (Result) then
   --  Success path
   Value : constant Civil := Civil_Result.Value (Result);
   --  Use value...
else
   --  Error path
   Put_Line ("Operation failed");
end if;
```

### Pattern 2: Extract Error Information

```ada
Result : constant Zone_ID_Result.Result :=
  Zone_ID_Pkg.From_String ("Invalid/Zone/Name");

if Zone_ID_Result.Is_Error (Result) then
   Err : constant Error_Type := Zone_ID_Result.Error_Info (Result);

   --  Get error kind
   Kind : constant Error_Kind := Err.Kind;

   --  Get error message
   Message : constant String := Error_Strings.To_String (Err.Message);

   Put_Line ("Error (" & Error_Kind'Image (Kind) & "): " & Message);
end if;
```

### Error Kinds

| Error Kind | Description | Example |
|------------|-------------|---------|
| `Validation_Error` | Invalid input data | Malformed datetime string |
| `Timezone_Error` | Timezone operation failed | Unknown zone ID |
| `Overflow_Error` | Arithmetic overflow | Instant out of range |
| `Ambiguous_Time_Error` | DST fall-back ambiguity | 1:30 AM on DST end |
| `Gap_Time_Error` | DST spring-forward gap | 2:30 AM on DST start |
| `IO_Error` | I/O operation failed | Cannot read timezone file |
| `Internal_Error` | Internal library error | Unexpected state |

### Why No Exceptions?

Zoneinfo follows functional programming principles:

- **Explicit error handling** - All errors visible in type signatures
- **Railway-oriented programming** - Chain operations with bind/map
- **No hidden control flow** - No surprise exceptions
- **Composable** - Errors are first-class values
- **SPARK-friendly** - Exception-free code is easier to verify

---

## Running Tests

```bash
# Run all tests (unit + integration)
make test-all

# Build all test executables
make build-tests

# Run unit tests only
./test/bin/unit_runner

# Run integration tests only
./test/bin/integration_runner

# Build specific test category
alr build --release test_unit_runner
alr build --release test_integration_runner
```

**Test Coverage:**
- **356 unit tests** - Domain, Application, Infrastructure, API layers
- **154 integration tests** - Cross-layer functionality
- **510 total tests** - All passing

**Expected Output:**
```
Running Unit Tests...
Domain Tests: 142 passed
Application Tests: 86 passed
Infrastructure Tests: 74 passed
API Tests: 54 passed

Total: 356/356 tests passed
```

---

## Build Profiles

Zoneinfo supports multiple build profiles for different use cases:

### Available Profiles

| Profile | Description | Use When |
|---------|-------------|----------|
| **development** | Debug symbols, runtime checks, assertions | Daily development |
| **release** | Optimizations, no debug symbols | Production use |
| **validation** | All checks, overflow detection | Pre-release validation |

### Using Profiles

```bash
# Development (default)
alr build
alr build --validation development

# Release (optimized)
alr build --release
alr build --validation release

# Validation (all checks)
alr build --validation validation

# SPARK verification
make spark-check
```

### Profile Comparison

| Feature | Development | Release | Validation |
|---------|-------------|---------|------------|
| Optimization | -O0 | -O2 | -O2 |
| Debug Symbols | Yes | No | Yes |
| Assertions | On | Off | On |
| Overflow Checks | On | Off | On |
| Range Checks | On | On | On |

---

## Common Issues

### Q: Parse.From_ISO_8601 returns error for valid string

**A:** Ensure the string follows ISO 8601 format exactly:
- Date separator: `-` (hyphen)
- Time separator: `:` (colon)
- Date/time separator: `T` (uppercase)
- Example: `2025-12-15T14:30:00`

```ada
--  ✅ Correct
Result := Parse.From_ISO_8601 ("2025-12-15T14:30:00");

--  ❌ Wrong (lowercase 't')
Result := Parse.From_ISO_8601 ("2025-12-15t14:30:00");

--  ❌ Wrong (space instead of 'T')
Result := Parse.From_ISO_8601 ("2025-12-15 14:30:00");
```

### Q: Timezone discovery fails with IO_Error

**A:** Ensure the IANA timezone database is installed and accessible:

```bash
# macOS
ls /usr/share/zoneinfo/America/New_York

# Linux
ls /usr/share/zoneinfo/America/New_York

# Windows
# Use TZif with custom data path
```

### Q: Duration arithmetic returns error

**A:** Instant arithmetic (not Duration arithmetic) returns Result types:

```ada
--  ✅ Correct - Instant + Duration returns Result
Result : constant Instant_Result.Result := Some_Instant + Some_Duration;

--  ✅ Correct - Duration + Duration returns Duration
Sum : constant Duration_Type := D1 + D2;

--  ❌ Wrong - trying to extract value from Duration (not a Result)
Value : constant Duration_Type := Duration_Result.Value (D1 + D2);
```

### Q: Format.To_ISO_8601 returns truncated string

**A:** Use `To_String` to convert bounded strings to String:

```ada
--  ✅ Correct
Datetime_Bounded : constant Datetime_String := Format.To_ISO_8601 (Civil_Time);
Datetime_Str     : constant String := Format.To_String (Datetime_Bounded);
Put_Line (Datetime_Str);

--  ❌ Wrong (bounded string doesn't display correctly with Put_Line)
Put_Line (String (Format.To_ISO_8601 (Civil_Time)));
```

### Q: Zone_ID_Pkg.From_String fails for valid zone

**A:** Zone IDs must match IANA timezone database exactly (case-sensitive):

```ada
--  ✅ Correct
Zone := Zone_ID_Pkg.From_String ("America/New_York");

--  ❌ Wrong (incorrect capitalization)
Zone := Zone_ID_Pkg.From_String ("America/new_york");

--  ❌ Wrong (underscore instead of slash)
Zone := Zone_ID_Pkg.From_String ("America_New_York");
```

---

## Next Steps

### Explore Full Documentation

- **[Software Requirements Specification](formal/software_requirements_specification.md)** - Detailed requirements
- **[Software Design Specification](formal/software_design_specification.md)** - Architecture details
- **[Software Test Guide](formal/software_test_guide.md)** - Testing strategy

### Dive Deeper into Architecture

- **[Architecture Enforcement](guides/architecture_enforcement.md)** - Layer dependency rules
- **[Error Handling Strategy](guides/error_handling_strategy.md)** - Result monad deep dive
- **[Build Profiles](guides/build_profiles.md)** - Multi-platform configuration

### API Reference

API documentation is embedded in source files:
- `src/api/zoneinfo-api.ads` - Main API types
- `src/api/parse/zoneinfo-api-parse.ads` - Parsing operations
- `src/api/format/zoneinfo-api-format.ads` - Formatting operations
- `src/api/operations/zoneinfo-api-operations.ads` - Pure operations
- `src/api/discovery/zoneinfo-api-discovery.ads` - Discovery operations

---

**License:** BSD-3-Clause<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.
