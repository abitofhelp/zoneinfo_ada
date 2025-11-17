# IANA Head-to-Head Validation Testing

This directory contains the head-to-head validation test suite for comparing ZoneInfo (Ada) against Python's zoneinfo reference implementation.

## Purpose

Validate that ZoneInfo (Ada) produces identical results to Python's authoritative IANA timezone library across all operations, timezones, and edge cases.

## Directory Structure

```
test/validation/
├── README.md           # This file
├── python/             # Python reference implementation
│   └── iana_validator.py
├── ada/                # Ada validation test runner
│   ├── validation.gpr
│   └── validation_runner.adb
└── results/            # Test results and difference logs
```

## Requirements

### Software Requirements

- **Python**: 3.9+ (for `zoneinfo` module)
- **Ada Compiler**: GNAT FSF 14.2+ or GNAT Pro 25.0+
- **Build System**: Alire 2.0+
- **TZData Version**: 2025b (both implementations)

### Performance Comparison Configuration

**Python Script vs Compiled Executable**:
- Python script: ~338ms per operation
- PyInstaller compiled exe: ~3,322ms per operation (10x slower)

**Reason for Script Usage**: PyInstaller executables incur significant startup overhead due to unpacking bundled libraries at runtime. For fair performance comparison, we use the **Python script** (not compiled exe) against the **compiled Ada executable**.

**Comparison**: Ada compiled exe vs Python interpreted script
- This is actually FAVORABLE to Python (interpreted is faster than PyInstaller exe)
- Provides realistic real-world comparison
- Both implementations use production-ready deployment methods

### Verifying TZData Versions

Both implementations MUST use the same tzdata version for valid comparisons.

**Python:**
```bash
python3 test/validation/python/iana_validator.py get_version
```

Expected output:
```json
{
  "command": "get_version",
  "success": true,
  "tzdata_version": "2025b",
  "python_version": "3.13.7 ..."
}
```

**System tzdata:**
```bash
cat /var/db/timezone/zoneinfo/+VERSION  # macOS
cat /usr/share/zoneinfo/+VERSION        # Linux
```

Expected: `2025b`

## Setup

### 1. Verify Python Installation

```bash
python3 --version  # Should be 3.9+
python3 -c "import zoneinfo; print('zoneinfo available')"
```

### 2. Test Python Validator

```bash
# Make executable
chmod +x test/validation/python/iana_validator.py

# Test basic operations
python3 test/validation/python/iana_validator.py get_version
python3 test/validation/python/iana_validator.py get_offset America/New_York 2024-01-15T12:00:00
python3 test/validation/python/iana_validator.py is_ambiguous America/New_York 2024-11-03T01:30:00
```

### 3. Build Ada Validation Runner

```bash
# From project root
alr build
alr exec -- gprbuild -P test/validation/ada/validation.gpr
```

### 4. Run Validation Tests

```bash
# From project root
./bin/validation/validation_runner
```

## Python Validator Usage

The Python validator (`iana_validator.py`) provides a command-line interface for all timezone operations.

### Commands

**Get tzdata version:**
```bash
python3 test/validation/python/iana_validator.py get_version
```

**Get UTC offset:**
```bash
python3 test/validation/python/iana_validator.py get_offset <zone> <datetime>

# Example:
python3 test/validation/python/iana_validator.py get_offset America/New_York 2024-01-15T12:00:00
```

**Check ambiguous time:**
```bash
python3 test/validation/python/iana_validator.py is_ambiguous <zone> <datetime>

# Example (fall back):
python3 test/validation/python/iana_validator.py is_ambiguous America/New_York 2024-11-03T01:30:00
```

**Check gap time:**
```bash
python3 test/validation/python/iana_validator.py is_gap <zone> <datetime>

# Example (spring forward):
python3 test/validation/python/iana_validator.py is_gap America/New_York 2024-03-10T02:30:00
```

**List all timezones:**
```bash
python3 test/validation/python/iana_validator.py list_zones
```

**Find timezones by pattern:**
```bash
python3 test/validation/python/iana_validator.py find_pattern <substring>

# Example:
python3 test/validation/python/iana_validator.py find_pattern "New_York"
```

**Find timezones by region:**
```bash
python3 test/validation/python/iana_validator.py find_region <region>

# Example:
python3 test/validation/python/iana_validator.py find_region Europe
```

### Output Format

All commands return JSON for easy integration:

```json
{
  "command": "get_offset",
  "success": true,
  "offset_seconds": -18000,
  "offset_str": "-05:00",
  "tzname": "EST"
}
```

Error responses:
```json
{
  "command": "get_offset",
  "success": false,
  "error": "Invalid zone: America/Invalid",
  "error_type": "ZoneInfoNotFoundError"
}
```

## Ada Validation Runner

The Ada validation runner (`validation_runner.adb`) executes comprehensive tests comparing ZoneInfo against expected Python results.

### Test Categories

**Category 1: Database Integrity**
- Zone existence verification
- Major timezone lookups
- Database connectivity

**Category 2: UTC Offset Calculations**
- Standard time offsets
- DST offsets
- Historical offsets
- Offset calculations across DST boundaries

**Category 3: DST Transition Detection**
- Ambiguous time detection (fall back)
- Gap time detection (spring forward)
- Transition boundary cases

**Category 4: Time Conversions**
- Timezone-to-timezone conversions
- Roundtrip conversion accuracy
- DST boundary conversions

### Running Tests

```bash
# Run all validation tests
./bin/validation/validation_runner

# Expected output:
========================================================
  IANA Head-to-Head Validation Test Suite
  ZoneInfo (Ada) vs Python zoneinfo
========================================================

TZData Version: 2025b (both implementations)

...

========================================================
  VALIDATION TEST SUMMARY
========================================================
Tests run:       8
Tests passed:    8
Tests failed:    0
Differences:     0

[SUCCESS] All validation tests passed!
ZoneInfo (Ada) matches Python zoneinfo reference.
========================================================
```

### Exit Codes

- **0**: All tests passed
- **1**: One or more tests failed

Use in CI/CD:
```bash
./bin/validation/validation_runner && echo "Validation passed" || echo "Validation failed"
```

## Known Issues and Limitations

### Zone Enumeration

**Issue**: `Get_Timezone_Count` and `Find_Timezones_By_Pattern("")` currently return 0 or incomplete results.

**Impact**: Cannot enumerate all timezones in the database.

**Workaround**: Individual zone lookups work correctly. Tests verify specific zones instead of counting.

**Status**: Known limitation in current TZif integration. Does not affect core timezone operations.

### Duplicate Results

**Issue**: Pattern searches may return duplicate zone names.

**Impact**: Zone counts may be inaccurate.

**Workaround**: Deduplicate results when needed.

## Test Results

### Current Status (2025-11-13)

- **TZData Version**: 2025b (verified identical)
- **Total Tests**: 8
- **Passed**: 8 ✅
- **Failed**: 0
- **Differences Found**: 0

### Validated Operations

- ✅ Zone existence (America/New_York, Europe/London, Asia/Tokyo)
- ✅ UTC offset calculations (standard time and DST)
- ✅ Ambiguous time detection (DST fall back)
- ✅ Gap time detection (DST spring forward)
- ✅ Time zone conversions

## Expanding Test Coverage

To add more test cases to the validation suite:

### 1. Add Python Test Cases

Edit `python/iana_validator.py` to add new operations or test data.

### 2. Add Ada Test Cases

Edit `ada/validation_runner.adb`:

```ada
-- Add new test in appropriate category section
declare
   Zone_Result : constant ZoneInfo.Zone_Id_Result :=
     Domain.Value_Object.Zone_Id.Result.Create ("Your/Zone");
   Test_Time : constant Ada.Calendar.Time := ...;
begin
   -- Perform operation
   -- Compare against expected Python result
   Test_Result (condition, "Test description");
end;
```

### 3. Rebuild and Run

```bash
alr exec -- gprbuild -P test/validation/ada/validation.gpr
./bin/validation/validation_runner
```

## Future Enhancements

### Phase 1: Direct Python Integration
- Call Python validator directly from Ada tests
- Automatic result comparison
- Detailed difference logging

### Phase 2: Comprehensive Test Cases
- All 600+ timezones
- Historical dates (pre-1970)
- Future dates (post-2038)
- All DST transitions in database
- Edge cases and boundary conditions

### Phase 3: Performance Testing
- Measure operation latency
- Compare Ada vs Python performance
- Identify optimization opportunities

### Phase 4: Continuous Validation
- CI/CD integration
- Automated nightly runs
- Regression detection
- Historical trend tracking

## Troubleshooting

### Python zoneinfo not found

**Error**: `ModuleNotFoundError: No module named 'zoneinfo'`

**Solution**: Upgrade Python to 3.9+ or install `backports.zoneinfo`:
```bash
python3 -m pip install backports.zoneinfo
```

### TZData version mismatch

**Error**: Different results between Ada and Python

**Solution**: Verify both use same tzdata version:
```bash
python3 test/validation/python/iana_validator.py get_version
cat /var/db/timezone/zoneinfo/+VERSION
```

### Ada build failures

**Error**: Compilation errors in validation_runner

**Solution**: Ensure ZoneInfo library builds successfully first:
```bash
alr build
alr exec -- gprbuild -P test/validation/ada/validation.gpr
```

## References

- [Python zoneinfo documentation](https://docs.python.org/3/library/zoneinfo.html)
- [IANA Time Zone Database](https://www.iana.org/time-zones)
- [TZif Format Specification (RFC 8536)](https://datatracker.ietf.org/doc/html/rfc8536)
- [Software Test Guide](../../docs/software_test_guide.md)

---

**Document Version**: 1.0.0
**Last Updated**: 2025-11-13
**Status**: Active
**Copyright**: © 2025 Michael Gardner, A Bit of Help, Inc.
**License**: BSD-3-Clause
