# Software Test Guide (STG)

**Version:** 2.1.0
**Date:** December 14, 2025  
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>  
**Status:** Released  

---

## 1. Introduction

### 1.1 Purpose

This Software Test Guide (STG) describes the testing strategy, test structure, and execution procedures for **Hybrid_Lib_Ada**.

### 1.2 Scope

This document covers:
- Test architecture and organization
- Unit and integration test suites
- Test execution procedures
- Test framework usage
- Adding new tests
- Example programs

### 1.3 References

- Software Requirements Specification (SRS)
- Software Design Specification (SDS)
- [All About Our API](../guides/all_about_our_api.md)

---

## 2. Test Strategy

### 2.1 Test Categories

| Category | Location | Purpose |
|----------|----------|---------|
| Unit Tests | `test/unit/` | Test individual packages in isolation |
| Integration Tests | `test/integration/` | Test cross-layer interactions |
| Example Programs | `examples/` | Runnable demonstrations of library usage |

See CHANGELOG for current test counts per release.

### 2.2 Testing Philosophy

- **Result Monad Testing**: All error paths tested via Result inspection
- **Mock-Based Isolation**: Infrastructure mocked for unit tests
- **No Exceptions**: Tests verify no exceptions raised
- **Deterministic**: Same inputs always produce same outputs
- **Full Coverage**: All public APIs tested

---

## 3. Test Organization

### 3.1 Directory Structure

```
test/
├── bin/                          # Compiled test executables
│   ├── unit_runner
│   ├── integration_runner
│   └── test_*.adb executables
│
├── common/                       # Shared test infrastructure
│   ├── test_framework.ads        # Result tracking, summaries
│   └── test_framework.adb
│
├── unit/                         # Unit test sources
│   ├── unit_tests.gpr            # GPR project
│   ├── unit_runner.adb           # Main test runner
│   ├── test_domain_error_result.adb
│   ├── test_domain_person.adb
│   ├── test_application_command_greet.adb
│   ├── test_application_usecase_greet.adb
│   └── test_api_operations.adb
│
├── integration/                  # Integration test sources
│   ├── integration_tests.gpr     # GPR project
│   ├── integration_runner.adb    # Main test runner
│   └── test_api_greet.adb
│
└── python/                       # Python-based tests (arch_guard)
    └── test_arch_guard_ada.py
```

### 3.2 Naming Conventions

| Element | Convention | Example |
|---------|------------|---------|
| Test file | `test_<layer>_<package>.adb` | `test_domain_person.adb` |
| Test name | Descriptive, action-result | "Create valid name - Is_Ok" |
| Mock prefix | `Mock_` | `Mock_Writer_Success` |
| Runner | `<category>_runner.adb` | `unit_runner.adb` |

### 3.3 GPR Projects

Tests use `hybrid_lib_ada_internal.gpr` which provides unrestricted access to all packages (no Library_Interface restrictions):

```ada
--  test/unit/unit_tests.gpr

with "../../hybrid_lib_ada_internal.gpr";

project Unit_Tests is
   for Source_Dirs use (".", "../common");
   for Object_Dir use "../../obj/test/unit";
   for Exec_Dir use "../bin";

   for Main use
     ("unit_runner.adb",
      "test_domain_error_result.adb",
      ...);

   package Compiler is
      for Default_Switches ("Ada") use
         Hybrid_Lib_Ada_Internal.Compiler'Default_Switches("Ada");
   end Compiler;
end Unit_Tests;
```

---

## 4. Test Framework

### 4.1 Framework Overview

The shared test framework (`test/common/test_framework.ads`) provides:
- Test result tracking
- Category summaries
- Grand total reporting
- Color-coded output

### 4.2 API

```ada
package Test_Framework is

   --  Track grand totals across all test suites
   procedure Register_Results (Total : Natural; Passed : Natural);

   --  Get cumulative results
   function Grand_Total_Tests return Natural;
   function Grand_Total_Passed return Natural;

   --  Reset counters (for test runner)
   procedure Reset;

   --  Print color-coded category summary
   function Print_Category_Summary
     (Category_Name : String;
      Total         : Natural;
      Passed        : Natural) return Integer;  --  0=success, 1=failure

end Test_Framework;
```

### 4.3 Usage Pattern

```ada
procedure Test_My_Package is
   Total_Tests  : Natural := 0;
   Passed_Tests : Natural := 0;

   procedure Run_Test (Name : String; Passed : Boolean) is
   begin
      Total_Tests := Total_Tests + 1;
      if Passed then
         Passed_Tests := Passed_Tests + 1;
         Put_Line ("[PASS] " & Name);
      else
         Put_Line ("[FAIL] " & Name);
      end if;
   end Run_Test;

begin
   Put_Line ("========================================");
   Put_Line ("Testing: My.Package");
   Put_Line ("========================================");

   --  Test cases
   Run_Test ("Test name", Condition);
   ...

   --  Register with framework
   Test_Framework.Register_Results (Total_Tests, Passed_Tests);
end Test_My_Package;
```

---

## 5. Test Execution

### 5.1 Running All Tests

```bash
make test-all
```

### 5.2 Running Specific Suites

```bash
# Unit tests only
make test-unit

# Integration tests only
make test-integration
```

### 5.3 Expected Output

```
========================================
     HYBRID_LIB_ADA UNIT TEST SUITE
========================================

========================================
Testing: Domain.Error.Result
========================================

[PASS] Ok construction - Is_Ok returns true
[PASS] Ok construction - Is_Error returns false
...

========================================
        GRAND TOTAL - ALL UNIT TESTS
========================================
Total tests:   [N]
Passed:        [N]
Failed:        0

########################################
###                                  ###
###    UNIT TESTS: SUCCESS           ###
###    All [N] tests passed!         ###
###                                  ###
########################################
```

*Note: [N] represents the current test count. See CHANGELOG for actual values.*

---

## 6. Test Details

### 6.1 Unit Tests

#### 6.1.1 Domain Layer Tests

**test_domain_error_result.adb**
- Package Under Test: `Domain.Error.Result`
- Tests Ok/Error construction, value extraction, multiple instances

**test_domain_person.adb**
- Package Under Test: `Domain.Value_Object.Person`
- Tests valid names, empty names, too-long names, special characters, Unicode

**test_domain_option.adb**
- Package Under Test: `Domain.Value_Object.Option`
- Tests Some/None construction, value extraction, Or_Else

#### 6.1.2 Application Layer Tests

**test_application_command_greet.adb**
- Package Under Test: `Application.Command.Greet`
- Tests command creation, name storage, round-trip

**test_application_usecase_greet.adb**
- Package Under Test: `Application.Usecase.Greet`
- Uses mock writer for isolation
- Tests valid execution, error propagation, writer failures

#### 6.1.3 API Layer Tests

**test_api_operations.adb**
- Package Under Test: `Hybrid_Lib_Ada.API.Operations`
- Uses mock writer
- Tests SPARK-safe generic instantiation

### 6.2 Integration Tests

**test_api_greet.adb**
- Tests full stack through `Hybrid_Lib_Ada.API`
- Uses real Console_Writer adapter
- Tests complete workflow end-to-end

### 6.3 Example Programs

See Section 9 for example program details.

---

## 7. Writing New Tests

### 7.1 Template

```ada
pragma Ada_2022;
--  ======================================================================
--  Test_My_Package
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Tests for My.Package functionality.
--  ======================================================================

with Ada.Text_IO; use Ada.Text_IO;
with Test_Framework;
with My.Package;

procedure Test_My_Package is
   Total_Tests  : Natural := 0;
   Passed_Tests : Natural := 0;

   procedure Run_Test (Name : String; Passed : Boolean) is
   begin
      Total_Tests := Total_Tests + 1;
      if Passed then
         Passed_Tests := Passed_Tests + 1;
         Put_Line ("[PASS] " & Name);
      else
         Put_Line ("[FAIL] " & Name);
      end if;
   end Run_Test;

begin
   Put_Line ("========================================");
   Put_Line ("Testing: My.Package");
   Put_Line ("========================================");

   --  Test cases here
   Run_Test ("Feature works", My.Package.Feature = Expected);

   --  Register results
   Test_Framework.Register_Results (Total_Tests, Passed_Tests);
end Test_My_Package;
```

### 7.2 Adding to GPR

```ada
--  test/unit/unit_tests.gpr
for Main use
  ("unit_runner.adb",
   ...
   "test_my_package.adb");  -- Add here
```

### 7.3 Mock Patterns

```ada
--  State for mock
Captured_Message : Unbounded_String;
Write_Call_Count : Natural := 0;
Mock_Should_Fail : Boolean := False;

function Mock_Writer (Message : String) return Unit_Result.Result is
begin
   Write_Call_Count := Write_Call_Count + 1;

   if Mock_Should_Fail then
      return Unit_Result.Error
        (Kind    => IO_Error,
         Message => "Mock failure");
   end if;

   Captured_Message := To_Unbounded_String (Message);
   return Unit_Result.Ok (Unit_Value);
end Mock_Writer;

procedure Reset_Mock is
begin
   Captured_Message := Null_Unbounded_String;
   Write_Call_Count := 0;
   Mock_Should_Fail := False;
end Reset_Mock;
```

---

## 8. Traceability

### 8.1 Requirements to Tests

| Requirement | Test File |
|-------------|-----------|
| FR-01.1 (Person) | test_domain_person.adb |
| FR-01.4 (Error) | test_domain_error_result.adb |
| FR-01.5 (Result) | test_domain_error_result.adb |
| FR-02.1 (Command) | test_application_command_greet.adb |
| FR-02.2 (Use Case) | test_application_usecase_greet.adb |
| FR-02.3 (Port) | test_application_usecase_greet.adb |
| FR-03.1 (Adapter) | test_api_greet.adb |
| FR-04.1 (Facade) | test_api_greet.adb |
| FR-04.4 (Operations) | test_api_operations.adb |

### 8.2 Layer Coverage

| Layer | Test Files |
|-------|-----------|
| Domain | test_domain_*.adb |
| Application | test_application_*.adb |
| API | test_api_*.adb |

See CHANGELOG for current test counts per layer and total.

---

## 9. Example Programs

Hybrid_Lib_Ada includes runnable example programs in the `examples/` directory that demonstrate library usage.

### 9.1 Basic Greeting

**File:** `examples/basic_greeting.adb`

**Purpose:** Demonstrates the simplest library usage - create a command, execute it, check the result.

**Build:**
```bash
alr exec -- gprbuild -P examples/examples.gpr
```

**Run:**
```bash
./examples/bin/basic_greeting
```

**Expected Output:**
```
=== Basic Greeting Example ===

Hello, World!
Greeting executed successfully!

=== End Example ===
```

### 9.2 Error Handling

**File:** `examples/error_handling.adb`

**Purpose:** Demonstrates Result monad error handling with validation errors (empty name, name too long).

**Run:**
```bash
./examples/bin/error_handling
```

**Expected Output:**
```
=== Error Handling Example ===

Test 1: Valid name 'Alice'
Hello, Alice!
  Result: OK - Greeting printed

Test 2: Empty name ''
  Result: ERROR - VALIDATION_ERROR: Name cannot be empty

Test 3: Name too long (150 chars)
  Result: ERROR - VALIDATION_ERROR: Name exceeds maximum length

=== End Example ===
```

### 9.3 Building Examples

Examples are built via the `examples/examples.gpr` project:

```bash
# Build examples
alr exec -- gprbuild -P examples/examples.gpr

# Executables in examples/bin/
ls examples/bin/
```

---

## 10. Test Maintenance

### 10.1 When to Update

- New package added → Add corresponding test file
- API changed → Update affected tests
- Bug fixed → Add regression test
- Error handling changed → Update error tests

### 10.2 Quality Guidelines

- Test names must be descriptive
- One assertion per test preferred
- Mock state reset between tests
- No test interdependencies

### 10.3 CI Integration

Tests run automatically on:
- Push to main branch
- Pull request creation
- Manual workflow dispatch

---

## 11. Appendices

### A. Exit Codes

| Code | Meaning |
|------|---------|
| 0 | All tests passed |
| 1 | One or more tests failed |

### B. Change History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 2.1.0 | 2025-12-14 | Michael Gardner | Remove hardcoded metrics per documentation standards; metrics now in CHANGELOG |
| 2.0.0 | 2025-12-09 | Michael Gardner | Complete regeneration for v2.0.0; added Section 9 Example Programs |
| 1.0.0 | 2025-11-29 | Michael Gardner | Initial release |
