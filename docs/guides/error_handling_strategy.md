# Error Handling Test Strategy

**Version:** 1.0.0-rc2  
**Date:** November 16, 2025  
**SPDX-License-Identifier:** BSD-3-Clause  
**License File:** See the LICENSE file in the project root.  
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.  
**Status:** Unreleased  


## Problem Statement

**Coverage Analysis Reveals:** The codebase has comprehensive happy-path testing (68% coverage) but **systematic gaps in error handling**:

- ✅ Features work correctly with valid input
- ❌ Error paths rarely exercised
- ❌ Exception handling not tested
- ❌ Invalid input validation incomplete
- ❌ I/O failure scenarios missing

**This is a common pattern in Ada codebases** - the type system catches many errors at compile time, leading developers to focus on the happy path. But production systems need robust error handling!

---

## Error Handling Categories to Test

### 1. **Parsing & Data Validation Errors**

**Current Gap:** Parser assumes all input is valid ZoneInfo format

**Test Coverage Needed:**
```ada
-- Invalid file format
- Empty file
- Wrong magic number (not "ZoneInfo")
- Invalid version byte
- Corrupted header checksums
- Mismatched counts (header says 10, file has 5)
- Truncated files (ends mid-record)
- Oversized files (suspicious data)
- Malformed binary integers
- Invalid UTF-8 in zone names
- Out-of-range values

-- Logical errors
- Transitions not in chronological order
- Duplicate transition times
- Invalid UTC offsets (> ±24 hours)
- Circular references in zone data
```

### 2. **I/O & File System Errors**

**Current Gap:** Tests assume files always exist and are readable

**Test Coverage Needed:**
```ada
-- File access errors
- File not found
- Permission denied (read-only, no access)
- Directory instead of file
- Symbolic link to non-existent file
- File locked by another process
- Disk full (write operations)
- Network filesystem timeout

-- Path errors
- Invalid path characters
- Path too long
- Relative vs absolute path handling
- Parent directory doesn't exist
- Case sensitivity issues
```

### 3. **Domain Validation Errors**

**Current Gap:** Domain objects constructed with valid data only

**Test Coverage Needed:**
```ada
-- Zone ID validation
- Empty zone ID
- Zone ID too long (> max length)
- Invalid characters in zone ID
- Null zone ID

-- Epoch validation
- Negative epochs (before 1970)
- Far future epochs (year 9999+)
- Epoch overflow
- Invalid Unix timestamp

-- Offset validation
- UTC offset out of range
- Fractional offsets (invalid)
- Null offsets

-- Transition validation
- Transition without corresponding info
- Info without corresponding transition
- Overlapping transitions
```

### 4. **Resource & State Errors**

**Current Gap:** Tests don't stress resource limits

**Test Coverage Needed:**
```ada
-- Memory pressure
- Very large zone database (1000+ zones)
- Large individual files (100+ MB)
- Memory allocation failures
- Cache overflow scenarios

-- Concurrency (if applicable)
- Concurrent file reads
- Cache invalidation race conditions
- Thread-safe operations
```

### 5. **Error Propagation & Recovery**

**Current Gap:** Error handling exists but isn't tested

**Test Coverage Needed:**
```ada
-- Result monad error paths
- Is_Error branches
- Error message extraction
- Error type discrimination
- Error recovery strategies

-- Exception handling
- Constraint_Error paths
- Storage_Error handling
- Program_Error scenarios
- User-defined exceptions
```

---

## Implementation Plan

### Phase 1: Parser Error Tests (Priority 1)

**File:** `test/integration/test_starterlib_parser_errors.adb`

**Test Structure:**
```ada
-- Test invalid file formats
Test_Empty_File
Test_Invalid_Magic_Number
Test_Corrupted_Header
Test_Truncated_File
Test_Invalid_Version
Test_Malformed_Integers
Test_Count_Mismatch
Test_Invalid_Transition_Times

-- Expected: All return Error results, not crashes
-- Verify: Meaningful error messages
```

**Expected Impact:** +15% coverage

---

### Phase 2: Repository Error Tests (Priority 2)

**Files:**
- `test/integration/test_repository_errors.adb` (new)
- Expand `test_load_source.adb`

**Test Structure:**
```ada
-- File system errors
Test_File_Not_Found
Test_Permission_Denied
Test_Invalid_Path
Test_Directory_Not_File
Test_Corrupted_Database

-- Search errors
Test_Pattern_No_Matches
Test_Invalid_Regex_Syntax
Test_Empty_Database

-- Expected: Graceful error handling, not crashes
```

**Expected Impact:** +8% coverage

---

### Phase 3: Domain Validation Tests (Priority 3)

**Files:**
- Expand `test/unit/test_zone_id.adb`
- Expand `test/unit/test_starterlib_data.adb`
- Create `test/unit/test_value_object_errors.adb`

**Test Structure:**
```ada
-- Boundary conditions
Test_Empty_Values
Test_Null_Values
Test_Max_Length_Values
Test_Out_Of_Range_Values

-- Invalid combinations
Test_Inconsistent_Data
Test_Circular_References
Test_Missing_Required_Fields
```

**Expected Impact:** +3% coverage

---

### Phase 4: Existing Test Enhancement (Priority 4)

**Strategy:** Add error scenarios to existing integration tests

**Examples:**
```ada
-- In test_find_by_id.adb
+ Test_Find_With_Empty_ID
+ Test_Find_With_Null_ID
+ Test_Find_With_Invalid_Characters

-- In test_find_by_pattern.adb
+ Test_Pattern_That_Matches_Nothing
+ Test_Malformed_Pattern
+ Test_Pattern_Too_Long

-- In test_find_by_regex.adb
+ Test_Invalid_Regex_Syntax (ALREADY EXISTS!)
+ Test_Catastrophic_Backtracking_Pattern
+ Test_Null_Regex
```

**Expected Impact:** +2% coverage

---

## Test Data Strategy

### Create Invalid Test Files

**Directory:** `test/data/invalid/`

```
invalid/
├── empty.zoneinfo                    # 0 bytes
├── wrong_magic.zoneinfo              # "XYZ" instead of "ZoneInfo"
├── truncated_header.zoneinfo         # Only 20 bytes of 44-byte header
├── truncated_data.zoneinfo           # Valid header, missing data
├── invalid_version.zoneinfo          # Version byte = 255
├── count_mismatch.zoneinfo           # Header: 10 transitions, Data: 5
├── corrupted_integers.zoneinfo       # Invalid binary data
├── negative_transition.zoneinfo      # Transition time overflow
├── unordered_transitions.zoneinfo    # Not chronological
└── oversized.zoneinfo                # Suspiciously large (100MB)
```

**How to Create:**
- Use hex editor to manually craft invalid files
- Script to generate corrupted variations
- Copy valid file and corrupt specific bytes

---

## Error Message Quality

### Requirements for Good Error Messages

```ada
-- BAD (current)
return Error ("Invalid file");

-- GOOD (target)
return Error ("ZoneInfo magic number validation failed: " &
              "expected 'ZoneInfo', got '" & Found & "' " &
              "at offset 0 in file " & Filename);
```

**Testing Error Messages:**
```ada
Assert (Contains (Error_Message, "expected 'ZoneInfo'"),
        "Error should mention expected magic number");
Assert (Contains (Error_Message, Filename),
        "Error should include filename");
```

---

## Coverage Goals by Error Category

| Category | Current | Target | Priority |
|----------|---------|--------|----------|
| Parser errors | 10% | 95% | HIGH |
| I/O errors | 5% | 90% | HIGH |
| Domain validation | 60% | 95% | MEDIUM |
| Resource errors | 0% | 70% | LOW |
| Error propagation | 40% | 90% | MEDIUM |

**Overall Target:** 95% coverage with comprehensive error handling

---

## Testing Anti-Patterns to Avoid

### ❌ Don't Do This:
```ada
-- Testing that an exception is raised
begin
   Parse_File ("invalid.zoneinfo");
   Assert (False, "Should have raised exception");
exception
   when others =>
      Assert (True);  -- Any exception passes!
end;
```

### ✅ Do This Instead:
```ada
-- Testing proper error handling via Result monad
Result := Parse_File ("invalid.zoneinfo");
Assert (Is_Error (Result),
        "Should return Error for invalid file");
Error_Info := Get_Error (Result);
Assert (Contains (Error_Info.Message, "invalid magic number"),
        "Error message should be specific");
Assert (Error_Info.Code = INVALID_FORMAT,
        "Error code should be INVALID_FORMAT");
```

---

## Metrics & Success Criteria

### Definition of Done:
- [ ] Coverage ≥ 95% overall
- [ ] Parser error paths ≥ 90% covered
- [ ] All `raise Constraint_Error` paths tested
- [ ] All `Is_Error` branches tested
- [ ] Meaningful error messages validated
- [ ] No uncaught exceptions in production code
- [ ] Error handling documented in code

### Continuous Monitoring:
```bash
# After each phase
make test-coverage
grep "STMT violations" coverage/summary.txt
grep "DECISION violations" coverage/summary.txt

# Track improvement
echo "$(date): $(grep 'lines (' coverage/summary.txt | head -1)" >> coverage_history.txt
```

---

## Long-term: Error Handling Standards

### Establish Project Guidelines:

1. **All public functions return Result types** (not exceptions for business logic)
2. **Exceptions only for programmer errors** (assertions, contracts)
3. **Error messages must be actionable** (tell user what to do)
4. **Error codes standardized** (domain error taxonomy)
5. **Every error path tested** (coverage requirement)

### Code Review Checklist:
- [ ] Does this function handle invalid input?
- [ ] Are error messages clear and actionable?
- [ ] Is the error path tested?
- [ ] Does this use Result monad correctly?
- [ ] Could this raise an unexpected exception?

---

## Next Steps

1. ✅ Create `ERROR_HANDLING_STRATEGY.md` (this document)
2. ⏭️ Implement Phase 1: Parser error tests
3. ⏭️ Implement Phase 2: Repository error tests
4. ⏭️ Implement Phase 3: Domain validation tests
5. ⏭️ Implement Phase 4: Enhance existing tests
6. ⏭️ Achieve 95% coverage target
7. ⏭️ Document error handling patterns for team

**Let's start with Phase 1!**
