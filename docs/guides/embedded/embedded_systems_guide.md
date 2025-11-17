# Embedded Systems and Restrictions in Ada 2022

**Version:** 1.0.0-rc2  
**Date:** November 16, 2025  
**SPDX-License-Identifier:** BSD-3-Clause
**License File:** See the LICENSE file in the project root.
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.  
**Status:** Living Document  

---

## Overview

This guide covers Ada restrictions for embedded and safety-critical systems, explaining how to design libraries that are **compatible** with embedded restrictions while remaining testable. Learn the proper pattern for restriction management in reusable libraries and applications.

**Core Philosophy:** Libraries should be *compatible* with restrictions, not *enforce* them—applications decide their own restriction policy.

---

## Table of Contents

1. [Quick Reference](#quick-reference)
2. [Understanding Ada Restrictions](#understanding-ada-restrictions)
3. [The Problem](#the-problem)
4. [The Solution](#the-solution)
5. [Profile-Based Restrictions](#profile-based-restrictions)
6. [Application Usage](#application-usage)
7. [Release Validation](#release-validation)
8. [Common Restrictions](#common-restrictions)
9. [Migration Guide](#migration-guide)

---

## Quick Reference

### For Library Authors

**DO NOT** put `pragma Restrictions` in library source code:
```ada
-- ❌ BAD: In library source
pragma Restrictions (No_Recursion);  -- Breaks AUnit tests!

-- ✅ GOOD: Document in comments
-- See config/profiles/*/restrictions.adc for recommended restrictions
```

**DO** provide restriction templates in profile directories:
```
config/profiles/
├── embedded_minimal/
│   ├── library_config.ads
│   └── restrictions.adc      # Restrictions for this profile
├── embedded_standard/
│   ├── library_config.ads
│   └── restrictions.adc
└── desktop/
    ├── library_config.ads
    └── restrictions.adc      # Empty (no restrictions)
```

### For Application Developers

Apply restrictions in your application (NOT in library source):

```bash
# Option 1: Copy as gnat.adc
cp library/config/profiles/embedded_standard/restrictions.adc ./gnat.adc
alr build

# Option 2: Reference in GPR
# In your project.gpr:
package Compiler is
   for Local_Configuration_Pragmas use "../library/config/profiles/embedded_standard/restrictions.adc";
end Compiler;

# Option 3: Compiler switch
alr exec -- gprbuild -gnatec=library/config/profiles/embedded_standard/restrictions.adc
```

---

## Understanding Ada Restrictions

### What Are Restrictions?

Ada restrictions are compile-time constraints that limit language features to enable verification, improve safety, or meet certification requirements (DO-178B/C, MISRA, etc.).

**Key Characteristic:** `pragma Restrictions` applies to the **entire partition** (executable), not just individual units or packages.

### Why Use Restrictions?

1. **Safety-Critical Systems** - Simplify certification analysis
2. **Real-Time Systems** - Predictable timing behavior
3. **Embedded Systems** - Reduce memory footprint
4. **Performance** - Enable aggressive compiler optimizations
5. **Verification** - Static analysis becomes tractable

### The Partition-Wide Problem

```ada
-- In library source (library.ads)
pragma Restrictions (No_Recursion);

-- In test file (test_library.adb)
with AUnit.Test_Suites;  -- ❌ ERROR! AUnit uses recursion

-- In application (main.adb)
-- ❌ ERROR! The restriction from library.ads applies here too!
```

**Problem:** Restrictions in library source force them on all code that depends on the library, including tests and applications that don't need restrictions.

---

## The Problem

### Challenge

How do we ensure an Ada library is compatible with embedded restrictions (like `No_Recursion`) while still being able to test it with AUnit (which uses recursion)?

### Constraints

1. Library must be **compatible** with embedded restrictions
2. Tests must work (AUnit uses recursion for test suite traversal)
3. `pragma Restrictions` in source code applies to **entire partition** (including tests)
4. Release process must verify restriction compliance

### Anti-Pattern: Restrictions in Library Source

```ada
-- ❌ ANTI-PATTERN: Do not do this!
-- src/my_library.ads
pragma Ada_2022;
pragma Restrictions (No_Recursion);  -- Breaks tests!

package My_Library is
   -- Library API
end My_Library;
```

**Problems**:
- Tests can't use AUnit (needs recursion)
- Applications are forced to follow library's restrictions
- Library becomes unusable in contexts that need different restrictions
- Violates separation of concerns

---

## The Solution

### Three-Part Pattern

1. **Library source: NO restrictions** - Unrestricted source code enables testing
2. **Profile restriction files** - Templates for different deployment scenarios
3. **Application enforcement** - Applications choose which restrictions to apply

### 1. Library Source Code: NO Restrictions

The library source code has **NO pragma Restrictions**:

```ada
-- src/my_library.ads
pragma Ada_2022;

-- ✅ GOOD: No pragma Restrictions in source!
-- See config/profiles/*/restrictions.adc for recommended restrictions

package My_Library is
   -- Library API
end My_Library;
```

**Why**: Keeps library source unrestricted so tests work, while applications can still enforce restrictions.

### 2. Profile-Based Restriction Files

Each profile provides a `restrictions.adc` file that **applications** use:

```
config/profiles/
├── embedded_minimal/
│   ├── library_config.ads    # Configuration constants
│   └── restrictions.adc       # Strictest restrictions
├── embedded_standard/
│   ├── library_config.ads
│   └── restrictions.adc       # Moderate restrictions
├── embedded_generous/
│   ├── library_config.ads
│   └── restrictions.adc       # Minimal restrictions
└── desktop/
    ├── library_config.ads
    └── restrictions.adc       # Empty (no restrictions)
```

**Example** `embedded_standard/restrictions.adc`:
```ada
-- Heap management
pragma Restrictions (No_Implicit_Heap_Allocations);
pragma Restrictions (No_Access_Parameter_Allocators);
pragma Restrictions (No_Coextensions);
pragma Restrictions (No_Anonymous_Allocators);
pragma Restrictions (No_Unchecked_Deallocation);

-- Stack management
pragma Restrictions (No_Recursion);

-- Concurrency
pragma Restrictions (No_Task_Allocators);
pragma Restrictions (Max_Tasks => 10);

-- Exception handling
pragma Restrictions (No_Exception_Propagation);
```

### 3. Application Enforcement

Applications decide which restrictions to apply based on their requirements.

---

## Profile-Based Restrictions

### Profile Hierarchy

**Desktop Profile** (No restrictions):
```ada
-- config/profiles/desktop/restrictions.adc
-- No restrictions for desktop/server applications
```

**Embedded Generous Profile** (Minimal restrictions):
```ada
-- config/profiles/embedded_generous/restrictions.adc
pragma Restrictions (No_Recursion);
pragma Restrictions (No_Implicit_Heap_Allocations);
```

**Embedded Standard Profile** (Moderate restrictions):
```ada
-- config/profiles/embedded_standard/restrictions.adc
pragma Restrictions (No_Implicit_Heap_Allocations);
pragma Restrictions (No_Recursion);
pragma Restrictions (No_Access_Parameter_Allocators);
pragma Restrictions (No_Coextensions);
pragma Restrictions (No_Anonymous_Allocators);
pragma Restrictions (No_Task_Allocators);
pragma Restrictions (Max_Tasks => 10);
```

**Embedded Minimal Profile** (Strictest restrictions):
```ada
-- config/profiles/embedded_minimal/restrictions.adc
pragma Restrictions (No_Implicit_Heap_Allocations);
pragma Restrictions (No_Allocators);
pragma Restrictions (No_Recursion);
pragma Restrictions (No_Exception_Propagation);
pragma Restrictions (No_Finalization);
pragma Restrictions (No_Nested_Finalization);
pragma Restrictions (No_Task_Hierarchy);
pragma Restrictions (Max_Tasks => 1);
```

---

## Application Usage

### Option 1: Copy as gnat.adc

The simplest approach for applications:

```bash
cd my_embedded_app/
cp library/config/profiles/embedded_standard/restrictions.adc ./gnat.adc
alr build  # Restrictions applied automatically
```

**Benefits**:
- Automatic - GNAT finds `gnat.adc` in current directory
- Simple - No GPR changes needed
- Standard - Follows Ada conventions

### Option 2: Reference in GPR

For more control in your project file:

```ada
-- my_app.gpr
project My_App is
   for Source_Dirs use ("src");
   for Object_Dir use "obj";
   for Main use ("main.adb");

   package Compiler is
      for Local_Configuration_Pragmas use
        "../library/config/profiles/embedded_standard/restrictions.adc";
   end Compiler;

end My_App;
```

**Benefits**:
- Explicit - Clear which restrictions are applied
- Flexible - Can switch profiles easily
- Documented - Part of project definition

### Option 3: Compiler Switch

For build system integration:

```bash
alr exec -- gprbuild -gnatec=library/config/profiles/embedded_standard/restrictions.adc
```

**Benefits**:
- Scriptable - Easy to automate
- CI/CD friendly - No source changes
- Testing - Easy to test with different profiles

---

## Release Validation

### Verify Restriction Compliance

The release process must verify library compliance with all embedded restrictions.

### Makefile Target

```makefile
# Makefile
.PHONY: verify-restrictions
verify-restrictions:
	@echo "Verifying embedded restrictions compliance..."
	$(ALR) exec -- $(GPRBUILD) -P library.gpr \
		-gnatec=config/profiles/embedded_standard/restrictions.adc \
		-XLIBRARY_PROFILE=embedded_standard
	@echo "✓ Library complies with embedded restrictions"

.PHONY: release-dry
release-dry: clean build test check verify-restrictions
	@echo "✓ Release dry run completed successfully!"
```

### Python Validation Script

```python
# scripts/validate_release.py
def check_restriction_compliance(self) -> ValidationResult:
    """Verify library complies with embedded restrictions"""
    profiles_to_test = ['embedded_minimal', 'embedded_standard', 'embedded_generous']

    for profile in profiles_to_test:
        print(f"  Testing {profile} restrictions...")

        result = subprocess.run([
            "alr", "exec", "--", "gprbuild",
            "-P", "library.gpr",
            f"-gnatec=config/profiles/{profile}/restrictions.adc",
            f"-XLIBRARY_PROFILE={profile}"
        ], capture_output=True)

        if result.returncode != 0:
            return ValidationResult.FAIL(f"Library violates {profile} restrictions")

    return ValidationResult.PASS("Library complies with all embedded restrictions")
```

### CI/CD Integration

```yaml
# .github/workflows/release.yml
jobs:
  validate-restrictions:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Install Alire
        run: |
          wget https://github.com/alire-project/alire/releases/download/v1.2.1/alr-1.2.1-bin-x86_64-linux.zip
          unzip alr-*.zip
          echo "$PWD/bin" >> $GITHUB_PATH

      - name: Verify Embedded Restrictions
        run: make verify-restrictions
```

---

## Common Restrictions

### Memory Management

| Restriction | Effect | Use Case |
|-------------|--------|----------|
| `No_Allocators` | No `new` keyword | Safety-critical |
| `No_Implicit_Heap_Allocations` | No automatic heap use | Embedded |
| `No_Access_Parameter_Allocators` | No `new` in access parameters | Safety |
| `No_Coextensions` | No nested access types | Safety |
| `No_Anonymous_Allocators` | No `new` without explicit type | Safety |
| `No_Unchecked_Deallocation` | No manual deallocation | Safety |

### Stack Management

| Restriction | Effect | Use Case |
|-------------|--------|----------|
| `No_Recursion` | No recursive calls | Real-time |
| `No_Implicit_Loops` | Explicit loops only | Verification |
| `Max_Asynchronous_Select_Nesting` | Limit select nesting | Real-time |

### Concurrency

| Restriction | Effect | Use Case |
|-------------|--------|----------|
| `No_Task_Allocators` | No dynamic tasks | Embedded |
| `No_Task_Hierarchy` | Flat task structure | Safety |
| `Max_Tasks` | Limit task count | Embedded |
| `Max_Protected_Entries` | Limit protected entries | Real-time |

### Exception Handling

| Restriction | Effect | Use Case |
|-------------|--------|----------|
| `No_Exception_Propagation` | Exceptions don't propagate | Safety |
| `No_Exceptions` | No exceptions at all | Safety-critical |

### Other

| Restriction | Effect | Use Case |
|-------------|--------|----------|
| `No_Finalization` | No controlled types | Embedded |
| `No_Nested_Finalization` | No nested controlled types | Safety |
| `No_Dependence` | Restrict package dependencies | Safety |

---

## Migration Guide

### For Existing Libraries

If your library currently has `pragma Restrictions` in source code, follow these steps:

#### Step 1: Remove Restrictions from Source

```ada
-- OLD: In your root package
pragma Restrictions (No_Recursion);  -- ❌ Remove this

-- NEW: Document in comments
-- ✅ See config/profiles/*/restrictions.adc for recommended restrictions
```

#### Step 2: Create Profile-Based Restriction Files

```bash
# Create profile directories
mkdir -p config/profiles/embedded_standard/
mkdir -p config/profiles/desktop/

# Create restriction file for embedded profile
cat > config/profiles/embedded_standard/restrictions.adc <<EOF
pragma Restrictions (No_Implicit_Heap_Allocations);
pragma Restrictions (No_Recursion);
pragma Restrictions (No_Access_Parameter_Allocators);
pragma Restrictions (No_Coextensions);
pragma Restrictions (No_Anonymous_Allocators);
pragma Restrictions (No_Unchecked_Deallocation);
EOF

# Desktop profile has no restrictions
touch config/profiles/desktop/restrictions.adc
```

#### Step 3: Add Release Validation

Add restriction compliance check to release process:

```python
# In scripts/validate_release.py
def check_restriction_compliance(self):
    """Verify library complies with embedded restrictions"""
    result = subprocess.run([
        "gprbuild", "-gnatec=config/profiles/embedded_standard/restrictions.adc"
    ])
    return result.returncode == 0
```

#### Step 4: Update Documentation

Update README and documentation:

```markdown
## Embedded Systems

This library is compatible with embedded restrictions. To use in an embedded application:

```bash
cp library/config/profiles/embedded_standard/restrictions.adc ./gnat.adc
alr build
```

See `docs/guides/embedded/embedded_systems_guide.md` for details.
```

### Testing the Migration

**Test without restrictions** (normal development):
```bash
$ make test
✓ Library builds
✓ All tests passed
```

**Test with restrictions** (verify compliance):
```bash
$ alr exec -- gprbuild -P library.gpr \
    -gnatec=config/profiles/embedded_standard/restrictions.adc \
    -XLIBRARY_PROFILE=embedded_standard
✅ PASSED: Library complies with embedded_standard restrictions
```

---

## Workflow Examples

### Development Workflow (No Restrictions)

```bash
# Normal development - fast, unrestricted
alr build
make test      # Tests work (AUnit uses recursion)
git commit -m "Add feature"
```

### Release Workflow (With Validation)

```bash
# Release verification includes restriction checks
make release-dry
  ✓ Library builds
  ✓ Tests pass
  ✓ Architecture validated
  ✓ Restriction compliance verified  # ← Ensures embedded compatibility
```

### Application Deployment (Restrictions Applied)

```bash
# Embedded application applies restrictions
cd my_embedded_app/
cp library/config/profiles/embedded_minimal/restrictions.adc ./gnat.adc
alr build  # ✅ Restrictions enforced in final binary
```

---

## Key Benefits

1. **Library source is unrestricted** → Tests work ✅
2. **Profiles provide restrictions** → Applications enforce as needed ✅
3. **Release process verifies** → Compliance guaranteed ✅
4. **Standard Ada practice** → Follows library conventions ✅
5. **Separation of concerns** → Library doesn't dictate application policy ✅

---

## References

- **GNAT Reference Manual** - Configuration Pragmas
- **Ada Reference Manual 13.12** - Pragma Restrictions and Pragma Profile
- **AUnit Cookbook** - Restricted Runtime Environments
- **DO-178B/C** - Software Considerations in Airborne Systems
- **MISRA Ada** - Guidelines for the Use of Ada in Critical Systems

---

## Summary

**The Pattern:**
- Library source has **NO** restrictions → Tests work
- Profiles provide restriction **templates** → Applications choose
- Release process **verifies** compliance → Quality guaranteed

**For Library Authors:**
- Don't put `pragma Restrictions` in source
- Provide restriction templates in profile directories
- Validate compliance in release process

**For Application Developers:**
- Choose profile matching your requirements
- Apply restrictions via `gnat.adc`, GPR, or compiler switch
- Library guarantees compatibility

**Next Steps**: See [Functional Programming Guide](../functional/functional_programming_guide.md) for Result-based error handling compatible with `No_Exception_Propagation`.
