# Zoneinfo Quick Start Guide

**Version:** 1.1.0<br>
**Date:** 2025-12-16<br>
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
- **TZif Library** ^3.0.3 (automatically fetched by Alire)
- **Functional Library** ^4.0.0 (automatically fetched by Alire)
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
     From_ISO_8601 ("2025-12-16T14:30:00");
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
Formatted: 2025-12-16T14:30:00
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
NY_Result : constant Zone_ID_Result.Result :=
  Zone_ID_Pkg.From_String ("America/New_York");

if Zone_ID_Result.Is_Ok (NY_Result) then
   NY_Zone : constant Zone_ID := Zone_ID_Result.Value (NY_Result);
   --  Convert to string
   Zone_Name : constant String := Zone_ID_Pkg.To_String (NY_Zone);
   --  Result: "America/New_York"
end if;
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

### Creating Civil Times

```ada
--  From components
Civil_Time : constant Civil := Civil_Pkg.Create
  (Year        => 2025,
   Month       => 12,
   Day         => 16,
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
Result := Parse.From_ISO_8601 ("2025-12-16T14:30:00");
Result := Parse.From_ISO_8601 ("2025-12-16T14:30:00.123456789");

--  With UTC offset
Result := Parse.From_ISO_8601_With_Offset ("2025-12-16T14:30:00-05:00");
Result := Parse.From_ISO_8601_With_Offset ("2025-12-16T14:30:00Z");

--  With zone ID
Result := Parse.From_ISO_8601_With_Zone ("2025-12-16T14:30:00[America/New_York]");

--  With offset AND zone
Result := Parse.From_ISO_8601_Full ("2025-12-16T14:30:00-05:00[America/New_York]");
```

### Parsing Dates and Times

```ada
--  Date only (time defaults to 00:00:00)
Date_Result := Parse.From_ISO_Date ("2025-12-16");

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
--  Result: "2025-12-16T14:30:00" or "2025-12-16T14:30:00.123456789"

--  Without nanoseconds
S := To_String (To_ISO_8601 (Civil_Time, Include_Nanos => False));
--  Result: "2025-12-16T14:30:00"
```

### Formatting with Timezones

```ada
Offset : constant Duration_Type := Duration_Pkg.From_Seconds (-18000);  --  -5 hours
Zone   : constant Zone_ID := Zone_ID_Pkg.UTC;  --  Use result-based From_String for others

--  With UTC offset
S := To_String (To_ISO_8601_With_Offset (Civil_Time, Offset));
--  Result: "2025-12-16T14:30:00-05:00"

--  With zone ID
S := To_String (To_ISO_8601_With_Zone (Civil_Time, Zone));
--  Result: "2025-12-16T14:30:00[UTC]"
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

---

## Timezone Discovery

The `Zoneinfo.API.Discovery` package provides timezone source discovery and querying.

**Note:** All zone listing operations return bounded arrays (`Zone_List` or `Search_Results`) for SPARK compatibility.

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

### Listing All Zones

Zone listing returns a bounded `Zone_List` containing up to `Max_Zone_List_Size` (750) zones:

```ada
--  List all zones from a source
Zones_Result : constant Zone_List_Result.Result :=
  List_All_Zones (Source, Descending => False);

if Zone_List_Result.Is_Ok (Zones_Result) then
   Zones : constant Zone_List := Zone_List_Result.Value (Zones_Result);

   Put_Line ("Found" & Zones.Count'Image & " zones:");
   for I in 1 .. Zones.Count loop
      Put_Line ("  " & To_String (Zones.Items (I)));
   end loop;
end if;
```

### Searching for Timezones

Search operations return a bounded `Search_Results` containing up to `Max_Search_Results` (100) matches:

```ada
--  Search by pattern (substring match)
Pattern_Result : constant Search_Results_Result.Result :=
  Find_By_Pattern ("York");

if Search_Results_Result.Is_Ok (Pattern_Result) then
   Results : constant Search_Results :=
     Search_Results_Result.Value (Pattern_Result);

   for I in 1 .. Results.Count loop
      Put_Line ("Found: " & To_String (Results.Items (I)));
   end loop;
end if;
--  Finds: America/New_York, etc.

--  Search by region (first component of zone ID)
Region_Result : constant Search_Results_Result.Result :=
  Find_By_Region ("America");
--  Finds: America/New_York, America/Chicago, America/Los_Angeles, etc.

--  Search by regex
Regex_Result : constant Search_Results_Result.Result :=
  Find_By_Regex ("America/.*York");
--  Finds: America/New_York
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
| `Overflow_Error` | Arithmetic overflow | Instant out of range, Zone_List full |
| `Ambiguous_Time_Error` | DST fall-back ambiguity | 1:30 AM on DST end |
| `Gap_Time_Error` | DST spring-forward gap | 2:30 AM on DST start |
| `IO_Error` | I/O operation failed | Cannot read timezone file |
| `Internal_Error` | Internal library error | Unexpected state |

### Result Operations

The Domain.Error.Result provides 7 essential operations:

```ada
Ok (Value)             -- Construct success
Error (Kind, Message)  -- Construct error
From_Error (Err)       -- Convert Error_Type to Result
Is_Ok (R)              -- Check success
Is_Error (R)           -- Check failure
Value (R)              -- Extract value (Pre: Is_Ok)
Error_Info (R)         -- Extract error (Pre: Is_Error)
```

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

# SPARK verification
make spark-check
```

**Test Coverage:**
- **335 unit tests** - Domain, Application, Infrastructure, API layers
- **154 integration tests** - Cross-layer functionality
- **489 total tests** - All passing

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

# Release (optimized)
alr build --release

# SPARK verification
make spark-check
make spark-prove
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
- Example: `2025-12-16T14:30:00`

```ada
--  Correct
Result := Parse.From_ISO_8601 ("2025-12-16T14:30:00");

--  Wrong (lowercase 't')
Result := Parse.From_ISO_8601 ("2025-12-16t14:30:00");

--  Wrong (space instead of 'T')
Result := Parse.From_ISO_8601 ("2025-12-16 14:30:00");
```

### Q: Timezone discovery fails with IO_Error

**A:** Ensure the IANA timezone database is installed and accessible:

```bash
# macOS/Linux
ls /usr/share/zoneinfo/America/New_York

# Windows
# Set TZIF_DATA_PATH environment variable to your timezone data location
```

### Q: List_All_Zones returns Overflow_Error

**A:** The bounded `Zone_List` has a maximum capacity of `Max_Zone_List_Size` (750 in standard profile). If the timezone database has more zones, an Overflow_Error is returned. Consider:
- Using a profile with larger capacity
- Filtering with `Find_By_Region` first

### Q: Zone_ID_Pkg.From_String fails for valid zone

**A:** Zone IDs must match IANA timezone database exactly (case-sensitive):

```ada
--  Correct
Zone := Zone_ID_Pkg.From_String ("America/New_York");

--  Wrong (incorrect capitalization)
Zone := Zone_ID_Pkg.From_String ("America/new_york");

--  Wrong (underscore instead of slash)
Zone := Zone_ID_Pkg.From_String ("America_New_York");
```

---

## Next Steps

### Explore Full Documentation

- **[Software Requirements Specification](formal/software_requirements_specification.md)** - Detailed requirements
- **[Software Design Specification](formal/software_design_specification.md)** - Architecture details
- **[Software Test Guide](formal/software_test_guide.md)** - Testing strategy

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
