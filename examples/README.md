# Zoneinfo Examples

This directory contains working examples demonstrating the Zoneinfo library's capabilities.

## Directory Structure

```
examples/
├── api/                      # Individual API examples (desktop, non-SPARK)
├── workflows/
│   ├── desktop/              # Real-world workflows using String (non-SPARK)
│   └── spark/                # Same workflows using Bounded_String (SPARK)
└── examples.gpr
```

### Desktop vs SPARK Examples

The `workflows/desktop/` and `workflows/spark/` directories contain the same use cases
implemented differently:

| Aspect | Desktop | SPARK |
|--------|---------|-------|
| String type | `String` (standard) | `Bounded_String` (stack only) |
| Conversion | Uses `To_String` helper | Uses bounded strings directly |
| I/O | `Ada.Text_IO.Put_Line` | No I/O (or SPARK-safe I/O) |
| Provability | Not formally verified | Can be formally proven |

Compare matching examples to see how SPARK constraints affect code structure.

## Building Examples

```bash
# Build all examples
alr exec -- gprbuild -P examples/examples.gpr

# Or from the examples directory
cd examples
alr exec -- gprbuild -P examples.gpr
```

Executables will be placed in `examples/bin/`.

## Workflow Examples (Desktop)

### 1. local_to_utc_to_zone - Timezone Conversion Basics

```bash
./examples/bin/local_to_utc_to_zone
```

The "hello world" of timezone programming:
- Get current local time
- Convert to UTC
- Convert to another timezone (Pacific/Fiji)
- Display times as ISO 8601 strings

### 2. epoch_and_duration - Instant and Duration Math

```bash
./examples/bin/epoch_and_duration
```

Working with epoch timestamps and durations:
- Parse ISO 8601 datetime string
- Parse ISO 8601 duration string (PT3H30M)
- Add duration to instant
- Convert between timezones
- Calculate duration between times
- Round-trip verification

### 3. zoned_arithmetic - Datetime Arithmetic

```bash
./examples/bin/zoned_arithmetic
```

Arithmetic operations on zoned datetime values:
- Add/subtract durations from instants
- Duration arithmetic (+, -, negation)
- DST transition detection
- Calculate elapsed time between instants

### 4. scheduling_across_zones - Multi-Zone Meeting Scheduling

```bash
./examples/bin/scheduling_across_zones
```

Real-world business scenario:
- Schedule a meeting at 2pm New York
- 90-minute meeting duration
- Show times for participants in Tokyo, London, Sydney
- Calculate timezone offset differences

## Key Features Demonstrated

- **Result Monad Pattern**: All operations return `Result[T, Error]`
- **NO EXCEPTIONS**: Functional error handling with explicit error paths
- **Zoneinfo.API Facade**: Public API for library operations
- **ISO 8601 Parsing/Formatting**: Full datetime and duration support
- **Duration Math**: Operator overloading for intuitive arithmetic
- **DST Handling**: Automatic daylight saving time transitions

## API Usage Pattern

```ada
with Zoneinfo.API;            use Zoneinfo.API;
with Zoneinfo.API.Desktop;    use Zoneinfo.API.Desktop;
with Zoneinfo.API.Parse;      use Zoneinfo.API.Parse;
with Zoneinfo.API.Format;     use Zoneinfo.API.Format;
with Zoneinfo.API.Operations; use Zoneinfo.API.Operations;

procedure Example is
   --  Parse datetime
   Civil_Result : constant Civil_Result.Result :=
     From_ISO_8601 ("2025-12-04T14:30:00");
begin
   if Civil_Result.Is_Ok (Civil_Result) then
      declare
         C : constant Civil := Civil_Result.Value (Civil_Result);
      begin
         --  Format to string (use To_String for desktop code)
         Put_Line (To_String (To_ISO_8601 (C)));
      end;
   end if;
end Example;
```

## See Also

- [Main README](../README.md) - Library overview and installation
- [Software Requirements Specification](../docs/formal/software_requirements_specification.md)
- [Software Design Specification](../docs/formal/software_design_specification.md)
- [Test Suite](../test/) - Comprehensive usage examples

## License

Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.

Licensed under the BSD-3-Clause License. See [../LICENSE](../LICENSE) for details.
