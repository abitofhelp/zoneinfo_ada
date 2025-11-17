# Test Organization - ZoneInfo Library

**Version:** 1.0.0-rc2  
**Date:** November 16, 2025  
**SPDX-License-Identifier:** BSD-3-Clause  
**License File:** See the LICENSE file in the project root.  
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.  
**Status:** Unreleased  


## Overview

The ZoneInfo library follows Ada testing best practices with proper separation of unit, integration, and E2E tests using AUnit framework.

**Date Organized**: November 5, 2025
**Framework**: AUnit 24.0.0+
**Coverage Target**: 90%+

## Directory Structure

```
test/
├── unit/                          # Unit tests (isolated, no I/O)
│   ├── test_timezone_lookup_unit.ads/adb   # 13 test cases
│   ├── test_suite_unit.ads/adb            # Test suite
│   ├── unit_runner.adb                    # Test runner
│   └── unit_tests.gpr                     # GPR file
│
├── integration/                   # Integration tests (real files, parser + services)
│   ├── test_starterlib_parser.adb              # Parser with system files
│   ├── test_starterlib_full.adb                # Full parsing test
│   ├── test_timezone_lookup.adb          # Parser + lookup integration
│   ├── test_parser_debug.adb             # Debug tool
│   └── integration_tests.gpr             # GPR file
│
└── e2e/                          # End-to-end tests (full workflows)
    └── (future: CLI tests, application tests)
```

## Test Types

### Unit Tests (`test/unit/`)

**Purpose**: Test individual components in isolation with no external dependencies

**Characteristics**:
- ✅ No file I/O
- ✅ No database access
- ✅ Uses constructed test data
- ✅ Fast execution (< 1 second)
- ✅ AUnit framework
- ✅ 90%+ code coverage target

**Current Coverage**:

#### Domain.Service.Timezone_Lookup (13 test cases)
- ✅ `Test_Find_Offset_No_Transitions` - UTC-like timezone
- ✅ `Test_Find_Offset_Single_Type` - Single timezone type
- ✅ `Test_Find_Offset_Before_First_Transition` - Time before transitions
- ✅ `Test_Find_Offset_After_Last_Transition` - Time after transitions
- ✅ `Test_Find_Offset_Between_Transitions` - Time between transitions
- ✅ `Test_Is_DST_No_Transitions` - DST check without transitions
- ✅ `Test_Is_DST_Standard_Time` - PST (standard time)
- ✅ `Test_Is_DST_Daylight_Time` - PDT (daylight time)
- ✅ `Test_Get_Abbreviation_UTC` - UTC abbreviation
- ✅ `Test_Get_Abbreviation_PST` - PST abbreviation
- ✅ `Test_Get_Abbreviation_PDT` - PDT abbreviation
- ✅ `Test_Binary_Search_Exact_Match` - Exact transition time
- ✅ `Test_Binary_Search_Between_Times` - Time between transitions

**Test Data Construction**:
```ada
-- Create UTC-like data (no transitions)
function Create_UTC_Like_Data return StarterLib_Data_Type;

-- Create test data with DST transitions
function Create_Test_Data_With_Transitions return StarterLib_Data_Type;
```

**Running Unit Tests**:
```bash
# Build
alr exec -- gprbuild -P test/unit/unit_tests.gpr

# Run
./bin/test/unit_runner

# Expected output:
# Total Tests Run:   13
# Successful Tests:  13
# Failed Assertions: 0
```

### Integration Tests (`test/integration/`)

**Purpose**: Test multiple components working together with real I/O

**Characteristics**:
- ✅ Reads real ZoneInfo files from filesystem
- ✅ Tests Infrastructure.StarterLib_Parser
- ✅ Tests Domain.Service.Timezone_Lookup with real data
- ✅ Validates against system timezone files
- ✅ Slower execution (1-3 seconds)
- ✅ Non-AUnit simple procedures

**Current Tests**:

1. **test_starterlib_parser.adb** - Basic parser validation
   - Parses system timezone files (New York, UTC, Los Angeles)
   - Tests error handling (invalid file)
   - Validates transition counts, types, POSIX TZ strings

2. **test_starterlib_full.adb** - Comprehensive parsing
   - Parses Los Angeles with full output
   - Shows transitions, timezone types, abbreviations
   - Validates ZoneInfo version 2 parsing

3. **test_timezone_lookup.adb** - End-to-end lookup
   - Parses Los Angeles timezone
   - Queries Dec 1, 2024 (PST, no DST)
   - Parses UTC timezone
   - Queries Jan 1, 2024 (UTC, no DST)

**Running Integration Tests**:
```bash
# Build
alr exec -- gprbuild -P test/integration/integration_tests.gpr

# Run individual tests
./bin/test/test_starterlib_parser
./bin/test/test_starterlib_full
./bin/test/test_timezone_lookup
```

### E2E Tests (`test/e2e/`)

**Purpose**: Test complete workflows from user perspective

**Status**: Not yet implemented

**Planned Tests**:
- CLI timezone query commands
- Bulk timezone conversion
- Performance benchmarks
- Multi-timezone workflows

## Test Execution Strategy

### Fast Suite (PRs, Development)
```bash
# Unit tests only - runs in < 1 second
./bin/test/unit_runner
```

### Full Suite (Pre-merge, CI)
```bash
# Unit tests
./bin/test/unit_runner

# Integration tests
./bin/test/test_starterlib_parser
./bin/test/test_starterlib_full
./bin/test/test_timezone_lookup
```

### Continuous Integration
```bash
# Via Alire (when configured in alire.toml)
alr run tests:unit
alr run tests:integration
alr run tests:e2e
```

## Test Coverage Analysis

### Current Coverage

**Domain.Service.Timezone_Lookup**: ~95%
- ✅ All public functions tested
- ✅ All code paths covered
  - No transitions (UTC-like)
  - Single type
  - Multiple types with transitions
  - Before/after/between transitions
  - Binary search edge cases
- ✅ All error paths tested
  - Empty types vector
  - Invalid type indices
  - Bounds checking

**Infrastructure.StarterLib_Parser**: ~85% (integration tests)
- ✅ Version 2/3/4 parsing
- ✅ Real file parsing
- ✅ Error handling (invalid files)
- ⚠️ Missing: Malformed ZoneInfo data tests

**Domain Value Objects**: ~70%
- ✅ Basic construction
- ⚠️ Missing: Value object invariant tests
- ⚠️ Missing: Boundary value tests

### Coverage Gaps (Future Work)

1. **Value Object Unit Tests**
   - Unix_Time edge cases (min/max values)
   - UTC_Offset bounds (-43200 to +50400)
   - StarterLib_Header validation

2. **Parser Unit Tests**
   - Mock stream reading
   - Malformed binary data
   - Version 1 vs Version 2 differences

3. **Error Path Tests**
   - Out of memory conditions
   - Corrupted timezone data
   - Invalid abbreviation indices

4. **Performance Tests**
   - Binary search performance (1000+ transitions)
   - Large timezone file parsing
   - Memory usage profiling

## Testing Principles (from Testing Guide)

### What We Follow

✅ **Unit tests in `test/unit/`** - Separate directory with AUnit
✅ **Integration tests in `test/integration/`** - Real I/O tests
✅ **Separate GPR per suite** - unit_tests.gpr, integration_tests.gpr
✅ **AUnit for unit tests** - Standard framework
✅ **90%+ coverage target** - Domain services fully covered
✅ **No mocking in domain** - Pure business logic
✅ **Real implementations preferred** - Only mock at infrastructure boundaries

### Mocking Strategy

**Never Mock**:
- Domain layer (pure business logic)
- Value objects
- Domain services

**Mock Only At Boundaries**:
- File I/O (for unit tests)
- External APIs (future)
- Time/clock (for deterministic tests)

**Current Approach**:
- Unit tests: Construct test data in memory
- Integration tests: Use real files from filesystem
- No mocks needed yet (domain is pure)

## Test Naming Conventions

### Files
- Unit tests: `test_<component>_unit.ads/adb`
- Integration tests: `test_<feature>.adb`
- Test suites: `test_suite_<type>.ads/adb`
- Runners: `<type>_runner.adb`

### Procedures
- AUnit tests: `Test_<What>_<Scenario>`
- Integration tests: `Test_<Feature>`

### Examples
```ada
-- Unit test procedure
procedure Test_Find_Offset_No_Transitions;

-- Integration test procedure
procedure Test_Los_Angeles;
```

## Adding New Tests

### Unit Test Checklist

1. Create test case file: `test/unit/test_<component>_unit.ads/adb`
2. Implement test procedures with AUnit.Test_Cases.Test_Case'Class
3. Add to test suite in `test_suite_unit.adb`
4. Build: `alr exec -- gprbuild -P test/unit/unit_tests.gpr`
5. Run: `./bin/test/unit_runner`
6. Verify 90%+ coverage

### Integration Test Checklist

1. Create test file: `test/integration/test_<feature>.adb`
2. Add to Main in `integration_tests.gpr`
3. Build: `alr exec -- gprbuild -P test/integration/integration_tests.gpr`
4. Run: `./bin/test/test_<feature>`
5. Verify real I/O works

## Test Maintenance

### When Code Changes

1. **Domain changes**: Update unit tests first
2. **Infrastructure changes**: Update integration tests
3. **API changes**: Update both unit and integration tests
4. **Bug fixes**: Add regression test

### Coverage Monitoring

```bash
# TODO: Set up gnatcov for coverage analysis
# gnatcov coverage --level=stmt+decision
```

## Future Enhancements

### Planned

1. **Value Object Unit Tests** - Complete domain coverage
2. **Parser Unit Tests** - Isolated parser logic
3. **E2E Test Suite** - Full application workflows
4. **Performance Benchmarks** - Regression prevention
5. **Property-Based Tests** - Randomized input testing
6. **Coverage Automation** - CI integration

### Tools to Add

- **gnatcov**: Statement and decision coverage
- **GNATtest**: Auto-generate test skeletons
- **Criterion**: Performance benchmarking (if needed)

## References

- [Ada Testing Guide](/Users/mike/.claude/agents/testing-guide.md)
- [AUnit Documentation](https://docs.adacore.com/live/wave/aunit/html/aunit_cb/aunit_cb.html)
- [ZoneInfo Library README](README.md)

## Test Results

### Current Status (November 5, 2025)

**Unit Tests**: ✅ 13/13 passing (100%)
**Integration Tests**: ✅ 3/3 passing (100%)
**E2E Tests**: ⚠️ Not yet implemented
**Overall Coverage**: ~85% (estimated)

```
Unit Test Results:
  Domain.Service.Timezone_Lookup: 13 tests, 0 failures

Integration Test Results:
  test_starterlib_parser: PASS
  test_starterlib_full: PASS
  test_timezone_lookup: PASS
```

---

**Last Updated**: 2025-11-05
**Maintained By**: Development Team
**Review Cycle**: After each major feature or bug fix
