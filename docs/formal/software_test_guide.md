# Software Test Guide

**Version:** 1.0.0<br>
**Date:** 2025-12-15<br>
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>
**Status:** Released

---

## 1. Introduction

### 1.1 Purpose

This Software Test Guide (STG) describes the test strategy, organization, and execution procedures for **Zoneinfo**, a timezone-aware datetime manipulation library for Ada 2022.

### 1.2 Scope

This document covers:
- Test strategy and philosophy (unit, integration, SPARK verification)
- Test organization and directory structure
- Test framework usage and conventions
- Test execution procedures and expected results
- Writing new tests (templates and guidelines)
- Requirements traceability matrix
- Test maintenance and quality guidelines

### 1.3 References

- Software Requirements Specification (SRS)
- Software Design Specification (SDS)
- Ada 2022 Reference Manual
- SPARK 2014 Reference Manual

---

## 2. Test Strategy

### 2.1 Test Categories

| Category | Location | Purpose | Count |
|----------|----------|---------|-------|
| **Unit Tests** | `test/unit/` | Test individual packages in isolation | 356 |
| **Integration Tests** | `test/integration/` | Test cross-layer interactions and adapters | 154 |
| **SPARK Checks** | Domain + Application | Formal verification (legality + flow analysis) | N/A |
| **Total** | | | **510** |

### 2.2 Testing Philosophy

**Zoneinfo follows these testing principles**:

1. **Comprehensive Coverage**: ≥90% statement+decision coverage for all layers
2. **Fast Feedback**: All tests run in <5 seconds for rapid development cycles
3. **Deterministic**: No flaky tests; all tests must be repeatable
4. **Isolated**: Unit tests have no external dependencies (use mocks)
5. **Integrated**: Integration tests verify real adapters with external libraries
6. **Formal**: SPARK verification provides mathematical proof of correctness for Domain + Application

### 2.3 Test Pyramid

```
                       ┌─────────────────┐
                       │  SPARK Checks   │  (Formal Verification)
                       │  Domain + App   │
                       └─────────────────┘
                      ┌───────────────────┐
                      │  Integration (154)│  (Cross-layer, Adapters)
                      └───────────────────┘
                  ┌──────────────────────────┐
                  │    Unit Tests (356)      │  (Individual Packages)
                  └──────────────────────────┘
```

**Rationale**:
- Unit tests: Fast, isolated, comprehensive coverage
- Integration tests: Verify real-world interactions
- SPARK checks: Formal proof of correctness (no runtime needed)

---

## 3. Test Organization

### 3.1 Directory Structure

```
test/
├── unit/                           # Unit tests (356 tests)
│   ├── test_domain_instant.adb
│   ├── test_domain_zoned.adb
│   ├── test_domain_civil.adb
│   ├── test_domain_duration.adb
│   ├── test_domain_zone_id.adb
│   ├── test_domain_error_result.adb
│   ├── test_domain_option.adb
│   ├── test_api_format.adb
│   ├── test_api_parse.adb
│   ├── test_console_writer.adb
│   └── unit_runner.adb             # Main test runner
│
├── integration/                     # Integration tests (154 tests)
│   ├── test_api_desktop.adb         # Full API composition root tests
│   ├── test_infrastructure_tzif.adb # tzif adapter tests
│   ├── test_epoch_conversions.adb   # Cross-layer conversion tests
│   └── integration_runner.adb       # Main test runner
│
├── python/                          # Python test infrastructure (submodule)
│   └── ...                          # Shared test utilities
│
├── bin/                             # Compiled test executables
│
└── config/                          # Test configuration files
```

### 3.2 Naming Conventions

| Item | Convention | Example |
|------|-----------|---------|
| **Test file** | `test_<package_name>.adb` | `test_domain_instant.adb` |
| **Test suite** | `<Package_Name>_Tests` | `Instant_Tests` |
| **Test case** | `Test_<Functionality>` | `Test_From_Unix_Epoch` |
| **Test runner** | `<type>_runner.adb` | `unit_runner.adb` |

### 3.3 GPR Projects

**Unit tests**:
```
test/unit/zoneinfo_unit_tests.gpr
```

**Integration tests**:
```
test/integration/zoneinfo_integration_tests.gpr
```

**Both projects depend on**:
- `zoneinfo.gpr` (main library)
- Test framework dependencies

---

## 4. Test Framework

### 4.1 Framework Overview

Zoneinfo uses a minimal test framework based on Ada's built-in capabilities:

- **No external test framework**: Uses Ada.Text_IO for output
- **Simple assertions**: Boolean checks with descriptive messages
- **Test suites**: Packages containing test procedures
- **Test runners**: Main programs that call all suites

### 4.2 Test Framework API

**Basic assertion**:
```ada
procedure Assert
  (Condition : Boolean;
   Message   : String;
   Test_Name : String);
--  If Condition is False, prints failure message and increments failure count
```

**Result assertions**:
```ada
procedure Assert_Ok
  (Result    : Result_Type;
   Message   : String;
   Test_Name : String);
--  Asserts Result.Is_Ok = True

procedure Assert_Error
  (Result    : Result_Type;
   Expected_Kind : Error_Kind;
   Message   : String;
   Test_Name : String);
--  Asserts Result is Error with expected kind
```

**Comparison assertions**:
```ada
procedure Assert_Equal
  (Actual, Expected : T;
   Message   : String;
   Test_Name : String);
--  Asserts Actual = Expected
```

### 4.3 Test Suite Template

```ada
pragma Ada_2022;
--  ======================================================================
--  Test_Package_Name
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Unit tests for Package.Name
--  ======================================================================

with Ada.Text_IO;
with Package.Under.Test;

package body Test_Package_Name is

   procedure Test_Some_Functionality is
      --  Arrange
      Expected : constant T := ...;

      --  Act
      Result := Package.Under.Test.Operation (...);

      --  Assert
      Assert (Result = Expected,
              "Operation should produce expected result",
              "Test_Some_Functionality");
   end Test_Some_Functionality;

   procedure Run_All_Tests is
   begin
      Ada.Text_IO.Put_Line ("Running Package_Name tests...");
      Test_Some_Functionality;
      Test_Another_Functionality;
      --  ... more tests
      Ada.Text_IO.Put_Line ("Package_Name tests complete.");
   end Run_All_Tests;

end Test_Package_Name;
```

---

## 5. Test Execution

### 5.1 Running All Tests

**Command** (from project root):
```bash
make test-all
```

**Expected output**:
```
Running unit tests...
[Unit test suite output]
Unit tests: 356 passed, 0 failed

Running integration tests...
[Integration test suite output]
Integration tests: 154 passed, 0 failed

TOTAL: 510 tests passed, 0 failed
```

### 5.2 Running Specific Test Suites

**Unit tests only**:
```bash
make test-unit
```

**Integration tests only**:
```bash
make test-integration
```

**Specific test file**:
```bash
cd test/unit
alr build
./bin/test_domain_instant
```

### 5.3 Running SPARK Verification

**SPARK legality checks** (Domain + Application):
```bash
make spark-check
```

**Expected output**:
```
Phase 1 of 2: generation of Global contracts ...
Phase 2 of 2: flow analysis and proof ...
Summary logged in gnatprove/gnatprove.out
```

**All checks should pass** (no errors, no warnings).

### 5.4 Running with Coverage

**Enable coverage** (requires GNATcoverage):
```bash
alr build --validation
make coverage
```

**View coverage report**:
```bash
open coverage/index.html
```

**Expected coverage**: ≥90% statement+decision coverage.

---

## 6. Test Details

### 6.1 Unit Tests

#### 6.1.1 Domain Layer Tests

| Test File | Purpose | Tests |
|-----------|---------|-------|
| **test_domain_instant.adb** | Instant value object tests | 34 |
| **test_domain_zoned.adb** | Zoned value object tests | 28 |
| **test_domain_civil.adb** | Civil value object tests | 42 |
| **test_domain_duration.adb** | Duration_Type tests | 58 |
| **test_domain_zone_id.adb** | Zone_ID tests | 18 |
| **test_domain_error_result.adb** | Result monad tests | 84 |
| **test_domain_option.adb** | Option monad tests | 56 |

**Example test coverage**:
- **test_domain_instant.adb**:
  - Construction: From_Unix_Epoch, From_Epoch_Nanos
  - Arithmetic: Add, Subtract, Diff (including overflow cases)
  - Comparison: =, <, <=, >, >=
  - Edge cases: Epoch constant, max/min values, overflow

#### 6.1.2 API Layer Tests

| Test File | Purpose | Tests |
|-----------|---------|-------|
| **test_api_format.adb** | ISO 8601 formatting tests | 24 |
| **test_api_parse.adb** | Zone_ID parsing tests | 18 |

#### 6.1.3 Infrastructure Layer Tests

| Test File | Purpose | Tests |
|-----------|---------|-------|
| **test_console_writer.adb** | Console output adapter tests | 14 |

### 6.2 Integration Tests

| Test File | Purpose | Tests |
|-----------|---------|-------|
| **test_api_desktop.adb** | Full API.Desktop composition root | 72 |
| **test_infrastructure_tzif.adb** | tzif adapter timezone operations | 54 |
| **test_epoch_conversions.adb** | Cross-layer Instant ↔ Civil ↔ Zoned | 28 |

**Integration test coverage**:
- Real clock reads (Ada.Calendar)
- Real timezone data (tzif library)
- DST transitions (spring-forward gaps, fall-back overlaps)
- Cross-layer data flow (API → Application → Infrastructure → Domain)

### 6.3 SPARK Verification

**Verified packages** (gnatprove --mode=check):
- All Domain layer packages (SPARK_Mode => On)
- All Application layer packages (SPARK_Mode => On)
- API.Operations (SPARK_Mode => On)

**Verification checks**:
- Legality: All SPARK language restrictions enforced
- Flow analysis: Initialization, data dependencies
- No runtime errors: (Future goal with --mode=prove)

---

## 7. Writing New Tests

### 7.1 Unit Test Checklist

When adding a new unit test:

- [ ] Create test file: `test_<package_name>.adb`
- [ ] Follow test suite template (Section 4.3)
- [ ] Test all public operations
- [ ] Test edge cases (empty, max, min, overflow)
- [ ] Test error conditions (Result.Is_Error cases)
- [ ] Add to appropriate GPR project file
- [ ] Add to test runner (`unit_runner.adb`)
- [ ] Verify ≥90% coverage for the package

### 7.2 Integration Test Checklist

When adding a new integration test:

- [ ] Identify cross-layer interaction to test
- [ ] Use real adapters (not mocks)
- [ ] Test with actual external dependencies (tzif, Ada.Calendar)
- [ ] Test both happy path and error paths
- [ ] Add to `test/integration/zoneinfo_integration_tests.gpr`
- [ ] Add to integration runner (`integration_runner.adb`)

### 7.3 Adding Tests to GPR Projects

**Example**: Adding `test_new_package.adb` to unit tests

**Edit `test/unit/zoneinfo_unit_tests.gpr`**:
```ada
for Source_Files use
  ("test_domain_instant.adb",
   "test_domain_zoned.adb",
   "test_new_package.adb",  -- ADD HERE
   "unit_runner.adb");
```

**Edit `test/unit/unit_runner.adb`**:
```ada
with Test_New_Package;

procedure Unit_Runner is
begin
   --  ... existing tests
   Test_New_Package.Run_All_Tests;  -- ADD HERE
end Unit_Runner;
```

### 7.4 Mock Patterns

**Example**: Mock clock for deterministic time tests

```ada
package Mock_Clock is
   --  Controllable clock for testing

   procedure Set_Time (T : Instant);
   --  Set the time that Now will return

   function Now return Instant_Result.Result;
   --  Returns the previously set time
end Mock_Clock;
```

**Usage in tests**:
```ada
--  Arrange
Mock_Clock.Set_Time (Some_Fixed_Instant);

--  Act
Result := SomeUseCase.Execute;  -- Uses Mock_Clock.Now

--  Assert
Assert (Result matches expected based on fixed time);
```

---

## 8. Traceability

### 8.1 Requirements to Tests Mapping

| Requirement | Test File(s) | Test Count |
|-------------|-------------|------------|
| **FR-01: Instant** | test_domain_instant.adb | 34 |
| FR-01.1 (Construction) | test_domain_instant.adb | 8 |
| FR-01.2 (Arithmetic) | test_domain_instant.adb | 12 |
| FR-01.3 (Comparison) | test_domain_instant.adb | 6 |
| FR-01.4 (Conversion) | test_domain_instant.adb | 8 |
| **FR-02: Zoned** | test_domain_zoned.adb, test_api_desktop.adb | 28 + 24 |
| **FR-03: Civil** | test_domain_civil.adb, test_epoch_conversions.adb | 42 + 12 |
| **FR-04: Duration** | test_domain_duration.adb | 58 |
| **FR-05: Zone_ID** | test_domain_zone_id.adb, test_api_parse.adb | 18 + 18 |
| **FR-06: Error Handling** | test_domain_error_result.adb | 84 |
| **FR-07: Clock Port** | test_api_desktop.adb | 18 |
| **FR-08: Timezone Port** | test_infrastructure_tzif.adb | 54 |
| **FR-09: Use Cases** | test_api_desktop.adb | 30 |
| **FR-10: Desktop Clock** | test_api_desktop.adb | 12 |
| **FR-12: TZif Adapter** | test_infrastructure_tzif.adb | 54 |
| **FR-13: Console Writer** | test_console_writer.adb | 14 |
| **FR-14-18: API Layer** | test_api_format.adb, test_api_parse.adb | 24 + 18 |

### 8.2 Coverage Summary

| Layer | Packages | Tests | Coverage |
|-------|----------|-------|----------|
| **Domain** | 13 | 320 | ≥90% |
| **Application** | 12 | 48 | ≥90% |
| **Infrastructure** | 5 | 68 | ≥85% |
| **API** | 6 | 74 | ≥90% |
| **Total** | 36 | 510 | ≥90% |

---

## 9. Test Maintenance

### 9.1 When to Update Tests

**Update tests when**:
- Adding new functionality (new FR)
- Changing existing behavior (update existing tests)
- Fixing bugs (add regression test)
- Refactoring (ensure tests still pass)

### 9.2 Test Quality Guidelines

**All tests MUST**:
- Have clear, descriptive names (`Test_From_Unix_Epoch_With_Valid_Input`)
- Follow Arrange-Act-Assert pattern
- Be independent (no test order dependencies)
- Be fast (<100ms per test)
- Have meaningful failure messages

**Example of good failure message**:
```ada
Assert (Result = Expected,
        "From_Unix_Epoch (1000, 500) should return " &
        "Instant with epoch_nanos = 1000500",
        "Test_From_Unix_Epoch_With_Valid_Input");
```

### 9.3 CI Integration

**GitHub Actions workflow** (`.github/workflows/ci.yml`):
```yaml
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: alire-project/setup-alire@v3
      - run: alr build
      - run: make test-all
      - run: make spark-check
```

**CI Requirements**:
- All 510 tests must pass
- SPARK checks must pass (no errors)
- Coverage must be ≥90% (validation profile)

---

## 10. Appendices

### Appendix A: Test Naming Convention Summary

| Element | Format | Example |
|---------|--------|---------|
| Test file | `test_<package>.adb` | `test_domain_instant.adb` |
| Test package | `Test_<Package>` | `Test_Domain_Instant` |
| Test procedure | `Test_<Operation>_<Condition>` | `Test_Add_Duration_Success` |
| Test suite runner | `Run_All_Tests` | `Run_All_Tests` |

### Appendix B: Make Targets Reference

| Target | Purpose |
|--------|---------|
| `make test-all` | Run all unit + integration tests |
| `make test-unit` | Run unit tests only |
| `make test-integration` | Run integration tests only |
| `make spark-check` | Run SPARK legality verification |
| `make spark-prove` | Run SPARK proof (future) |
| `make coverage` | Generate coverage report |
| `make clean` | Clean build artifacts |

### Appendix C: Test Count Breakdown

**Unit Tests (356 total)**:
- Domain.Value_Object.Instant: 34
- Domain.Value_Object.Zoned: 28
- Domain.Value_Object.Civil: 42
- Domain.Value_Object.Duration_Type: 58
- Domain.Value_Object.Zone_ID: 18
- Domain.Error.Result: 84
- Domain.Types.Option: 56
- API.Format: 24
- API.Parse: 18
- Infrastructure.Adapter.Console_Writer: 14

**Integration Tests (154 total)**:
- API.Desktop (full composition): 72
- Infrastructure.Adapter.Tzif: 54
- Epoch conversions (cross-layer): 28

**Total: 510 tests**

### Appendix D: Change History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-12-15 | Initial release - regenerated from source |

---

**Document Control:**
- Version: 1.0.0
- Last Updated: 2025-12-15
- Status: Released
