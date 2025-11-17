# ZoneInfo Library - Test Infrastructure

**Version:** 1.0.0-rc2  
**Date:** November 16, 2025  
**SPDX-License-Identifier:** BSD-3-Clause  
**License File:** See the LICENSE file in the project root.  
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.  
**Status:** Unreleased  


## Overview

Comprehensive test suite for the ZoneInfo timezone library targeting 95%+ code coverage using AUnit framework.

## Test Strategy

**Philosophy:**
- **Integration tests first** - Test real ZoneInfo file parsing with actual timezone data
- **Unit tests for calculations** - Domain services and value object operations
- **Minimal mocking** - Use real filesystem adapter with test data
- **AUnit framework** - Ada's standard testing framework
- **Coverage target: 95%+**

## Test Structure

```
test/
├── test.gpr                        # Test project file
├── aunit_test_runner.adb           # Main test runner
├── starterlib_test_suite.ads/adb         # Main test suite aggregator
├── domain/                          # Unit tests (pure logic)
│   ├── test_zone_id_suite.*        # Zone_Id validation, parsing
│   └── test_source_info_suite.*    # Source_Info ULID, validation
├── integration/                     # Integration tests (real files)
│   ├── test_find_by_id_suite.*     # Command 1: Find by exact ID
│   └── test_get_version_suite.*    # Command 4: Get version
└── fixtures/                        # Test data (real ZoneInfo files)
    └── zoneinfo/                    # Minimal test database
        ├── +VERSION                 # Version file
        ├── America/
        │   ├── New_York             # Real ZoneInfo file
        │   ├── Los_Angeles
        │   └── Phoenix
        ├── Europe/
        │   ├── London
        │   └── Paris
        └── UTC                      # Simple zone
```

## Test Coverage Breakdown

### Domain Layer (Unit Tests) - ~15% of coverage
- **Zone_Id**: Validation, conversion, bounded string operations
- **Source_Info**: ULID generation, path validation
- **Epoch_Seconds**: Range validation, conversion
- **Result monad**: Ok/Err creation, unwrap, map operations

### Infrastructure Repository (Integration Tests) - ~50% of coverage
- All 14 repository functions with real ZoneInfo files
- Parse ZoneInfo v2/v3 format
- Handle missing files (ZoneNotFound errors)
- Handle corrupted files (ParseError errors)
- Bounded string conversions at boundaries

### Use Cases (Covered via Integration) - ~30% of coverage
- Each use case tested via integration tests
- Since use cases are thin wrappers (renames), integration tests provide full coverage

### Error Paths - ~10% of coverage
- File not found scenarios
- Parse errors with malformed ZoneInfo data
- Invalid zone IDs
- Missing version files
- Cache import/export failures

## Test Cases Implemented

### Domain Tests (Unit)

**test_zone_id_suite.adb:**
1. `Test_Make_Zone_Id_Valid` - Valid zone ID creation
2. `Test_Make_Zone_Id_Empty` - Empty string validation fails
3. `Test_Make_Zone_Id_Too_Long` - Length limit (256 chars) enforced
4. `Test_Zone_Id_Equality` - Equality comparison works

**test_source_info_suite.adb:**
1. `Test_ULID_Generation` - ULID format is 26 characters
2. `Test_ULID_Uniqueness` - Sequential ULIDs are unique
3. `Test_Source_Info_Creation` - Source metadata creation

### Integration Tests (Real Files)

**test_find_by_id_suite.adb:**
1. `Test_Find_UTC` - Find UTC timezone
2. `Test_Find_America_New_York` - Find complex zone with transitions
3. `Test_Find_Case_Insensitive` - Case-insensitive lookup
4. `Test_Find_Invalid_Zone` - Error handling for missing zones

**test_get_version_suite.adb:**
1. `Test_Get_Version_System_DB` - Version retrieval from discovered source
2. `Test_Version_Format` - Version string format validation

## Running Tests

### Build Tests
```bash
cd test
gprbuild -p -P test.gpr
```

### Run Tests
```bash
./bin/aunit_test_runner
```

### With Coverage
```bash
# Build with coverage instrumentation
gprbuild -p -P test.gpr -Xcov=true

# Run tests
./bin/aunit_test_runner

# Generate coverage report
gnatcov coverage --level=stmt --annotate=html aunit_test_runner.trace
```

## Remaining Test Suites to Create

1. **test_get_transition_at_epoch_suite** - Get transition at specific epoch
2. **test_find_my_id_suite** - Find local system timezone
3. **test_list_all_zones_suite** - List all zones sorted
4. **test_find_by_pattern_suite** - Substring search with yield
5. **test_find_by_region_suite** - Region prefix match
6. **test_find_by_regex_suite** - Regex matching
7. **test_discover_sources_suite** - Discover all timezone sources
8. **test_load_source_suite** - Load source from path
9. **test_validate_source_suite** - Validate source accessibility
10. **test_import_cache_suite** - Import cache from file
11. **test_export_cache_suite** - Export cache to file

## Coverage Goals

- **95%+ Overall Coverage**
  - Domain layer: 100% (pure logic, fully testable)
  - Application layer: 95%+ (use cases are thin wrappers)
  - Infrastructure layer: 90%+ (adapter code, error paths)

## Test Data Requirements

### Fixtures Needed
- Real ZoneInfo files from system (`/usr/share/zoneinfo`)
- Test version file (`+VERSION`)
- Minimal zone set: UTC, America/New_York, America/Los_Angeles, America/Phoenix, Europe/London, Europe/Paris
- Malformed ZoneInfo file for parse error tests
- Empty directory for negative tests

### Environment
- Tests use system timezone database if available
- Falls back to test fixtures if system DB not found
- Tests are deterministic (no time-dependent behavior)

## Notes

### Current Status
- ✅ Test infrastructure created
- ✅ 2 domain test suites implemented
- ✅ 2 integration test suites implemented
- ⏳ GPR dependencies need resolution
- ⏳ 11 integration test suites remaining
- ⏳ Coverage measurement setup needed

### Build Issues
- Need to resolve AUnit GPR path in Alire environment
- GPR file needs proper dependency chain
- May need to build via `alr` instead of direct `gprbuild`

### Future Enhancements
- Add performance benchmarks
- Add fuzzing tests for ZoneInfo parser
- Add property-based tests (QuickCheck style)
- Add memory leak detection tests
- Add concurrency stress tests
