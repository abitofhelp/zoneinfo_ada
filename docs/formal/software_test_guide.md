# Software Test Guide

**Version:** 1.1.1<br>
**Date:** 2025-12-16<br>
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
| **Unit Tests** | `test/unit/` | Test individual packages in isolation | 335 |
| **Integration Tests** | `test/integration/` | Test cross-layer interactions and adapters | 154 |
| **SPARK Checks** | Domain + Application | Formal verification (legality + flow analysis) | N/A |
| **Total** | | | **489** |

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
                  │    Unit Tests (335)      │  (Individual Packages)
                  └──────────────────────────┘
```

---

## 3. Test Organization

### 3.1 Directory Structure

```
test/
├── unit/                           # Unit tests (335 tests)
│   ├── test_domain_instant.adb
│   ├── test_domain_zoned.adb
│   ├── test_domain_civil.adb
│   ├── test_domain_duration.adb
│   ├── test_domain_zone_id.adb
│   ├── test_domain_error_result.adb   # 7 essential operations
│   ├── test_api_format.adb
│   ├── test_api_parse.adb
│   └── unit_runner.adb             # Main test runner
│
├── integration/                     # Integration tests (154 tests)
│   ├── test_api_desktop.adb         # Full API composition root tests
│   ├── test_infrastructure_tzif.adb # tzif adapter tests
│   ├── test_epoch_conversions.adb   # Cross-layer conversion tests
│   └── integration_runner.adb       # Main test runner
│
└── bin/                             # Compiled test executables
```

### 3.2 Naming Conventions

| Item | Convention | Example |
|------|-----------|---------|
| **Test file** | `test_<package_name>.adb` | `test_domain_instant.adb` |
| **Test suite** | `<Package_Name>_Tests` | `Instant_Tests` |
| **Test case** | `Test_<Functionality>` | `Test_From_Unix_Epoch` |
| **Test runner** | `<type>_runner.adb` | `unit_runner.adb` |

---

## 4. Test Execution

### 4.1 Running All Tests

```bash
# Run all tests (unit + integration)
make test-all

# Build all test executables
make build-tests
```

### 4.2 Running Unit Tests

```bash
# Build and run unit tests
make test-unit

# Or run directly
./test/bin/unit_runner
```

**Expected Output:**
```
Running Unit Tests...
Domain Tests: 142 passed
Application Tests: 86 passed
Infrastructure Tests: 54 passed
API Tests: 53 passed

Total: 335/335 tests passed
```

### 4.3 Running Integration Tests

```bash
# Build and run integration tests
make test-integration

# Or run directly
./test/bin/integration_runner
```

**Expected Output:**
```
Running Integration Tests...
API.Desktop Tests: 45 passed
Infrastructure.TZif Tests: 62 passed
Cross-layer Tests: 47 passed

Total: 154/154 tests passed
```

### 4.4 SPARK Verification

```bash
# Legality check (fast - seconds)
make spark-check

# Full proof (slow - 60-90 minutes)
make spark-prove
```

**Expected SPARK Check Output:**
```
gnatprove --mode=check ...
Phase 1 of 2: generation of Global contracts ...
Phase 2 of 2: flow analysis and proof ...
Summary logged in /path/to/gnatprove.out
```

---

## 5. Writing New Tests

### 5.1 Unit Test Template

```ada
pragma Ada_2022;
--  ==========================================================================
--  Test_<Package_Name> - Unit tests for <Package> package
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause

with Ada.Text_IO; use Ada.Text_IO;
with <Package_Under_Test>;

procedure Test_<Package_Name> is
   use <Package_Under_Test>;

   Test_Count  : Natural := 0;
   Pass_Count  : Natural := 0;

   procedure Assert (Condition : Boolean; Message : String) is
   begin
      Test_Count := Test_Count + 1;
      if Condition then
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("FAIL: " & Message);
      end if;
   end Assert;

   --  Test Cases
   procedure Test_Basic_Operation is
   begin
      Assert (True, "Basic operation should work");
   end Test_Basic_Operation;

   procedure Test_Edge_Case is
   begin
      Assert (True, "Edge case should handle correctly");
   end Test_Edge_Case;

begin
   Put_Line ("Running <Package_Name> Tests...");

   Test_Basic_Operation;
   Test_Edge_Case;

   Put_Line ("Passed:" & Pass_Count'Image & "/" & Test_Count'Image);
end Test_<Package_Name>;
```

### 5.2 Integration Test Template

```ada
pragma Ada_2022;
--  ==========================================================================
--  Test_<Feature> - Integration tests for <Feature>
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause

with Ada.Text_IO; use Ada.Text_IO;
with Zoneinfo.API.Desktop;
with Zoneinfo.API.Discovery;

procedure Test_<Feature> is
   use Zoneinfo.API.Desktop;
   use Zoneinfo.API.Discovery;

   --  Test with real adapters and external dependencies
begin
   Put_Line ("Testing <Feature> with real adapters...");

   --  Test real integration scenarios
   declare
      Result : constant Instant_Result.Result := Now_UTC;
   begin
      if Instant_Result.Is_Ok (Result) then
         Put_Line ("PASS: Now_UTC returns valid instant");
      else
         Put_Line ("FAIL: Now_UTC failed");
      end if;
   end;
end Test_<Feature>;
```

### 5.3 Testing Result Types

```ada
--  Testing success case
declare
   R : constant My_Result.Result := My_Operation;
begin
   Assert (My_Result.Is_Ok (R), "Operation should succeed");
   Assert (My_Result.Value (R) = Expected, "Value should match");
end;

--  Testing error case
declare
   R : constant My_Result.Result := My_Failing_Operation;
begin
   Assert (My_Result.Is_Error (R), "Operation should fail");
   Assert (My_Result.Error_Info (R).Kind = Validation_Error,
           "Error should be Validation_Error");
end;
```

### 5.4 Testing Bounded Arrays (v1.1.0)

```ada
--  Testing Zone_List
declare
   Zones_Result : constant Zone_List_Result.Result :=
     List_All_Zones (Source);
begin
   Assert (Zone_List_Result.Is_Ok (Zones_Result), "Should list zones");

   declare
      Zones : constant Zone_List := Zone_List_Result.Value (Zones_Result);
   begin
      Assert (Zones.Count > 0, "Should have zones");
      Assert (Zones.Count <= Max_Zone_List_Size, "Should not exceed capacity");

      --  Test specific zone
      for I in 1 .. Zones.Count loop
         if To_String (Zones.Items (I)) = "America/New_York" then
            Assert (True, "Should contain America/New_York");
            exit;
         end if;
      end loop;
   end;
end;
```

---

## 6. Test Coverage by Layer

### 6.1 Domain Layer Tests

| Package | Test Count | Coverage |
|---------|------------|----------|
| Domain.Value_Object.Instant | 28 | Constructors, arithmetic, comparisons |
| Domain.Value_Object.Zoned | 18 | Create, accessors, With_Zone |
| Domain.Value_Object.Civil | 32 | All components, leap year, days-in-month |
| Domain.Value_Object.Duration_Type | 24 | Arithmetic, conversions |
| Domain.Value_Object.Zone_ID | 16 | From_String, To_String, UTC constant |
| Domain.Error.Result | 14 | 7 essential operations |
| Domain.Unit | 10 | Unit type operations |

### 6.2 Application Layer Tests

| Package | Test Count | Coverage |
|---------|------------|----------|
| Application.UseCase.Get_Now | 12 | Execute, error handling |
| Application.UseCase.Timezone_Ops | 28 | To_Civil, To_Zoned, edge cases |
| Application.UseCase.Discovery | 46 | List_All_Zones, Find_By_* patterns |

### 6.3 Infrastructure Layer Tests

| Package | Test Count | Coverage |
|---------|------------|----------|
| Infrastructure.Adapter.Desktop_Clock | 18 | Now_UTC, Now_Zoned |
| Infrastructure.Adapter.TZif | 36 | All tzif operations |

### 6.4 API Layer Tests

| Package | Test Count | Coverage |
|---------|------------|----------|
| Zoneinfo.API.Format | 22 | All formatting functions |
| Zoneinfo.API.Parse | 31 | All parsing functions, error cases |
| Zoneinfo.API.Operations | 16 | Instant/Duration arithmetic |

---

## 7. Requirements Traceability

### 7.1 Functional Requirements Coverage

| Requirement | Tests | Status |
|-------------|-------|--------|
| FR-01: Instant Value Object | test_domain_instant.adb | Covered |
| FR-02: Zoned Value Object | test_domain_zoned.adb | Covered |
| FR-03: Civil Value Object | test_domain_civil.adb | Covered |
| FR-04: Duration Value Object | test_domain_duration.adb | Covered |
| FR-05: Zone_ID Value Object | test_domain_zone_id.adb | Covered |
| FR-06: Zone Collections | test_domain_zone_id.adb | Covered |
| FR-07: Error Handling | test_domain_error_result.adb | Covered |
| FR-08: Clock Port | test_api_desktop.adb | Covered |
| FR-09: Timezone Port | test_infrastructure_tzif.adb | Covered |
| FR-10: Use Cases | test_api_desktop.adb | Covered |
| FR-17: API.Discovery | test_api_desktop.adb | Covered (bounded arrays) |

### 7.2 Non-Functional Requirements Coverage

| Requirement | Verification Method |
|-------------|---------------------|
| NFR-01: Performance | Manual benchmarks |
| NFR-02: Reliability | Error handling tests |
| NFR-03: Portability | CI multi-platform builds |
| NFR-04: Maintainability | Code review, linting |
| NFR-05: Usability | Example compilation |
| NFR-06: SPARK Verification | make spark-check |
| NFR-07: Testability | This test suite |

---

## 8. Troubleshooting

### 8.1 Common Test Failures

**Q: Tests fail with "tzif data not found"**

A: Ensure IANA timezone database is installed:
```bash
# macOS/Linux
ls /usr/share/zoneinfo/America/New_York

# Windows - set environment variable
export TZIF_DATA_PATH=/path/to/zoneinfo
```

**Q: SPARK check fails**

A: Ensure all Domain/Application specs have `SPARK_Mode => On`:
```ada
package My_Package
  with Preelaborate, SPARK_Mode => On
is
```

**Q: Test counts don't match**

A: Regenerate test runners if tests were added/removed:
```bash
make build-tests
```

---

## 9. CI/CD Integration

### 9.1 GitHub Actions Workflow

```yaml
test:
  runs-on: ubuntu-latest
  steps:
    - uses: actions/checkout@v4
    - name: Setup Alire
      uses: alire-project/setup-alire@v3
    - name: Build
      run: alr build
    - name: Run Tests
      run: make test-all
    - name: SPARK Check
      run: make spark-check
```

### 9.2 Test Artifacts

- `test/bin/` - Compiled test executables
- `gnatprove/` - SPARK proof logs
- Test output logs in CI artifacts

---

**Document Control:**
- Version: 1.1.0
- Last Updated: 2025-12-16
- Status: Released
