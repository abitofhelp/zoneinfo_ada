# ZoneInfo Examples

This directory contains practical examples demonstrating how to use the ZoneInfo library for timezone operations with Ada.Calendar integration.

## Available Examples

### Basic Operations

**`get_offset.adb`** - Basic UTC Offset Lookup
- Get UTC offset for a timezone at a specific date/time
- Simple demonstration of the core API
- **Usage**: `./bin/examples/get_offset <timezone> <ISO8601-datetime>`
- **Example**: `./bin/examples/get_offset America/New_York 2024-07-15T12:00:00`

**`timezone_info.adb`** - Timezone Information Query
- Query detailed information about a timezone
- Display UTC offset, DST status, timezone abbreviation
- **Usage**: `./bin/examples/timezone_info <timezone> <ISO8601-datetime>`
- **Example**: `./bin/examples/timezone_info Europe/London 2024-01-15T10:00:00`

**`search_timezones.adb`** - Search and Find Timezones
- Search timezones by pattern or region
- List all available timezones
- **Usage**: `./bin/examples/search_timezones`

### Ada.Calendar Integration Examples

**`time_arithmetic.adb`** - Time Calculations with Ada.Calendar
- Adding/subtracting Duration to/from Ada.Calendar.Time
- Converting local time to UTC and back
- Calculating future/past times across timezones
- **Demonstrates**:
  - `Time + Duration = Time`
  - `Time - Duration = Time`
  - `Time - Time = Duration`
  - Timezone conversions using UTC offsets
- **Usage**: `./bin/examples/time_arithmetic`

**`duration_calculations.adb`** - Working with Duration Values
- Computing time spans between events
- Converting Duration to hours/minutes/seconds
- Handling timezone-aware duration calculations
- **Real-world scenarios**:
  - Work hours calculation
  - Global meeting duration
  - Flight time across timezones
- **Usage**: `./bin/examples/duration_calculations`

**`timezone_conversion.adb`** - Practical Timezone Conversion
- Global team coordination examples
- International conference call scheduling
- Financial market trading hours
- **Real-world scenarios**:
  - Daily standup across SF, London, Bangalore
  - Stock exchange overlap analysis
  - Best time for global meetings
- **Usage**: `./bin/examples/timezone_conversion`

**`dst_disambiguation.adb`** - DST Transition Handling
- Complete guide to handling DST ambiguity (FR-10.3, FR-10.4, FR-10.5)
- Demonstrates all three disambiguation strategies
- Shows ambiguous times (fall-back) and gap times (spring-forward)
- Production-ready error handling patterns
- **Real-world scenarios**:
  - Handling "2 AM" during fall-back (occurs twice)
  - Detecting invalid times during spring-forward
  - Choosing correct offset for recurring events
  - Explicit user choice for ambiguous times
- **Usage**: `./bin/examples/dst_disambiguation`

### Comprehensive Testing

**`comprehensive_test.adb`** - Full API Test Suite
- Tests all ZoneInfo API operations
- Demonstrates error handling with Result types
- Validates functionality across multiple timezones
- **Usage**: `./bin/examples/comprehensive_test`

## Building Examples

### Build All Examples

```bash
# Build all examples (debug mode)
alr build

# Build all examples (release mode - optimized)
alr build -- -Xmode=release
alr exec -- gprbuild -P examples/examples.gpr -Xmode=release
```

### Build Individual Example

```bash
# Build specific example
alr exec -- gprbuild -P examples/examples.gpr -Xmode=release time_arithmetic.adb
```

## Running Examples

After building, executables are in `bin/examples/`:

```bash
# Run time arithmetic example
./bin/examples/time_arithmetic

# Run duration calculations
./bin/examples/duration_calculations

# Run timezone conversion scenarios
./bin/examples/timezone_conversion

# Run DST disambiguation examples
./bin/examples/dst_disambiguation

# Query specific timezone
./bin/examples/get_offset Asia/Tokyo 2024-07-15T15:00:00

# Search for timezones
./bin/examples/search_timezones
```

## Example Output

### Time Arithmetic Example

```
===========================================================
  Time Arithmetic with Ada.Calendar and ZoneInfo
===========================================================

=== Example 1: Adding Hours to a Time ===

Current time (2 PM): 2024-07-15 14:00:00.0
After adding 3 hours (5 PM): 2024-07-15 17:00:00.0

Note: Ada.Calendar.Time supports direct arithmetic:
  Time + Duration = Time
  Time - Duration = Time
  Time - Time = Duration

=== Example 2: Converting Between Timezones ===

Question: What time is it in Tokyo when it's 3 PM in New York?

New York local time: 2024-07-15 15:00:00.0
NY UTC offset: -14400.0 seconds

Converted to UTC: 2024-07-15 19:00:00.0

Tokyo local time: 2024-07-16 04:00:00.0
Tokyo UTC offset: 32400.0 seconds

Result: 3 PM in New York = 4 AM next day in Tokyo
(13 hour time difference in summer)
```

### Duration Calculations Example

```
=== Example 3: Global Meeting Duration ===

Question: A meeting starts at 10 AM London and ends at 5 PM Tokyo.
How long is the meeting?

Start: 10:00 AM London (GMT+0s)
End:    5:00 PM Tokyo (GMT+32400s)

Meeting duration: 8h 0m 0s (28800 seconds)

Note: Always convert to UTC before calculating durations
across timezones to avoid DST and offset errors!
```

## Key Concepts Demonstrated

### 1. Railway-Oriented Programming (Result Types)

All examples use functional error handling with Result types:

```ada
Zone_Result : constant ZoneInfo.Zone_Id_Result :=
  Domain.Value_Object.Zone_Id.Result.Create ("America/New_York");

if Domain.Value_Object.Zone_Id.Result.Is_Ok (Zone_Result) then
   -- Success path
else
   -- Error path
end if;
```

### 2. Ada.Calendar Integration

Examples show how to use ZoneInfo with Ada's standard time library:

```ada
-- Create a time
Local_Time : constant Ada.Calendar.Time :=
  Ada.Calendar.Time_Of (2024, 7, 15, 15.0 * 3600.0);  -- 3 PM

-- Get timezone offset
Offset_Duration : constant Duration := ...;  -- From ZoneInfo

-- Convert to UTC
UTC_Time : constant Ada.Calendar.Time := Local_Time - Offset_Duration;
```

### 3. Duration Arithmetic

Working with time spans:

```ada
-- Calculate time difference
Start_Time : constant Ada.Calendar.Time := ...;
End_Time : constant Ada.Calendar.Time := ...;
Meeting_Duration : constant Duration := End_Time - Start_Time;

-- Add duration to time
Future_Time : constant Ada.Calendar.Time := Start_Time + (2.5 * 3600.0);  -- +2.5 hours
```

### 4. Best Practices

Examples demonstrate production-ready patterns:

- ✅ Always convert to UTC before calculating cross-timezone durations
- ✅ Use Result types for error handling (no exceptions)
- ✅ Type-safe time arithmetic with Ada.Calendar
- ✅ Clear separation of local time vs UTC
- ✅ Proper handling of DST transitions

## Learning Path

**Recommended order for learning:**

1. **`get_offset.adb`** - Understand basic API usage
2. **`timezone_info.adb`** - Learn timezone queries
3. **`time_arithmetic.adb`** - Master Ada.Calendar integration
4. **`duration_calculations.adb`** - Work with time spans
5. **`timezone_conversion.adb`** - Apply to real-world scenarios
6. **`comprehensive_test.adb`** - See complete API coverage

## Common Use Cases

### Calculate Meeting Time Across Timezones

See `timezone_conversion.adb` Example 1 (Global Standup)

### Compute Flight Duration

See `duration_calculations.adb` Example 4 (Flight Time)

### Schedule International Events

See `timezone_conversion.adb` Example 3 (Conference Call)

### Convert Between Timezones

See `time_arithmetic.adb` Example 2 (Timezone Conversion)

## Troubleshooting

### Example Won't Build

```bash
# Ensure ZoneInfo library is built first
alr build

# Then build examples
alr exec -- gprbuild -P examples/examples.gpr
```

### Timezone Not Found

- Check timezone name spelling (case-sensitive)
- Use `search_timezones` to find valid timezone names
- Example: `America/New_York` (not `America/NewYork`)

### Time Conversion Seems Wrong

- Remember to account for DST (offsets change by season)
- Always convert through UTC for cross-timezone calculations
- Use `timezone_info` to verify UTC offset at specific date

## Related Documentation

- [ZoneInfo API Documentation](../docs/index.md)
- [Validation Test Results](../test/validation/results/COMPREHENSIVE_TEST_SUMMARY.md)
- [Software Architecture](../docs/guides/hybrid-architecture/)
- [Main README](../README.md)

---

**Copyright**: © 2025 Michael Gardner, A Bit of Help, Inc.
**License**: BSD-3-Clause
