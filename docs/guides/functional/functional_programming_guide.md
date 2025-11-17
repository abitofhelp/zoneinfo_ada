# Functional Programming in Ada 2022

**Version:** 1.0.0-rc2  
**Date:** November 16, 2025  
**SPDX-License-Identifier:** BSD-3-Clause
**License File:** See the LICENSE file in the project root.
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.  
**Status:** Living Document  

---

## Overview

This guide covers functional programming patterns in Ada 2022, focusing on **Railway-Oriented Programming** with algebraic data types for explicit error handling without exceptions.

**Core Philosophy:** Catch exceptions at boundaries and return typed values (`Result`, `Option`, `Either`) — no raises escaping your functions.

---

## Table of Contents

1. [Quick Reference](#quick-reference)
2. [Core Concepts](#core-concepts)
3. [Result Type](#result-type)
4. [Option Type](#option-type)
5. [Either Type](#either-type)
6. [Exception Boundaries](#exception-boundaries)
7. [Railway-Oriented Programming](#railway-oriented-programming)
8. [Advanced Patterns](#advanced-patterns)

---

## Quick Reference

### Common Operations

| Operation | Result | Option | Purpose |
|-----------|--------|--------|---------|
| `Ok(v)` / `Some(v)` | ✓ | ✓ | Create success value |
| `Err(e)` / `None` | ✓ | ✓ | Create failure/absence |
| `Is_Ok` / `Is_Some` | ✓ | ✓ | Check if has value |
| `Is_Err` / `Is_None` | ✓ | ✓ | Check if error/empty |
| `Map` | ✓ | ✓ | Transform value |
| `And_Then` | ✓ | ✓ | Chain fallible operations |
| `Map_Error` | ✓ | - | Transform error |
| `Unwrap_Or` | ✓ | ✓ | Provide default |
| `Recover` | ✓ | - | Handle errors |
| `With_Context` | ✓ | - | Add error context |

### Type Instantiation Template

```ada
pragma Ada_2022;

-- Error type for your domain
type Error_Kind is (IO_Error, Parse_Error, Validation_Error);
type Error is record
   Kind    : Error_Kind;
   Message : String;
end record;

-- Instantiate functional types
with Functional.Result;
with Functional.Option;

package Str_Result is new Functional.Result.Instance
   (T => String, E => Error);

package Int_Option is new Functional.Option.Instance
   (T => Integer);
```

---

## Core Concepts

### The Problem: Exception-Based Error Handling

Traditional Ada exception handling has issues:
- **Invisible control flow** - raises not in function signature
- **Loss of type safety** - exceptions are runtime, not compile-time
- **Difficult composition** - can't chain fallible operations easily
- **Poor testability** - harder to test all error paths

### The Solution: Algebraic Data Types

**Result<T, E>** - Represents a value that might fail:
```ada
type Result is (Ok, Err);
```

**Option<T>** - Represents a value that might be absent:
```ada
type Option is (Some, None);
```

**Either<L, R>** - Represents one of two possible values:
```ada
type Either is (Left, Right);
```

### Mental Model

Think of these types as **containers** that force you to handle both paths:

```
Result<String, Error>
    ├─ Ok("success")     ← Happy path
    └─ Err(error)        ← Error path

Option<Integer>
    ├─ Some(42)          ← Value present
    └─ None              ← Value absent
```

---

## Result Type

### When to Use

Use `Result<T, E>` for operations that can fail with a meaningful error:
- File I/O
- Parsing
- Validation
- Database queries
- Network requests

### Creating Results

```ada
-- Success case
function Parse_Config (Path : String) return Config_Result.Result is
begin
   Config := Load_From_File (Path);
   return Config_Result.Ok (Config);
exception
   when E : others =>
      return Config_Result.Err ((IO_Error,
         "Failed to load: " & Exception_Message (E)));
end Parse_Config;

-- Error case
function Validate_Age (Age : Integer) return Int_Result.Result is
begin
   if Age < 0 or Age > 150 then
      return Int_Result.Err ((Validation_Error, "Age out of range"));
   end if;
   return Int_Result.Ok (Age);
end Validate_Age;
```

### Checking Results

```ada
Config := Parse_Config ("app.toml");

if Config_Result.Is_Ok (Config) then
   -- Use Config_Result.Value (Config)
   Put_Line ("Loaded config");
else
   -- Use Config_Result.Error (Config)
   Put_Line ("Error: " & Config_Result.Error (Config).Message);
end if;
```

### Pattern: Add Context to Errors

```ada
function Add_Context (E : Error; Msg : String) return Error is
   ((E.Kind, E.Message & " :: " & Msg));

function With_File_Context is new Config_Result.With_Context
   (Append => Add_Context);

Config := With_File_Context (Config, "loading app.toml");
-- Error now includes: "Failed to load: ... :: loading app.toml"
```

---

## Option Type

### When to Use

Use `Option<T>` when a value might be absent (no error information needed):
- Dictionary lookups
- Array searches
- Optional configuration values
- Nullable fields

### Creating Options

```ada
function Find_User (ID : User_ID) return User_Option.Result is
begin
   for U of Users loop
      if U.ID = ID then
         return User_Option.Some (U);
      end if;
   end loop;
   return User_Option.None;
end Find_User;
```

### Providing Defaults

```ada
-- Eager default
Name : String := Str_Option.Unwrap_Or (Find_Name (ID), "Unknown");

-- Lazy default (computed only if None)
function Default_Name return String is ("Guest");
function Get_With_Default is new Str_Option.Unwrap_Or_With
   (F => Default_Name);
Name := Get_With_Default (Find_Name (ID));
```

### Converting Option to Result

```ada
function Not_Found return Error is
   ((Validation_Error, "User not found"));

function To_Res is new User_Option.To_Result
   (E => Error, T_Result => User_Result);

User := To_Res (Find_User (ID), Not_Found);
-- None becomes Err(Not_Found)
```

---

## Either Type

### When to Use

Use `Either<L, R>` for neutral disjunctions (not specifically success/error):
- Polymorphic return values
- Branching logic
- Type-level choices

### Example: Parse Result Can Be Int or Float

```ada
package Num_Either is new Functional.Either.Instance
   (L => Integer, R => Float);

function Parse_Number (S : String) return Num_Either.Either is
begin
   -- Try int first
   return Num_Either.Left (Integer'Value (S));
exception
   when Constraint_Error =>
      -- Try float
      return Num_Either.Right (Float'Value (S));
end Parse_Number;
```

---

## Exception Boundaries

### The Problem

Third-party libraries and Ada runtime operations raise exceptions. We want to catch these at boundaries and convert to typed Results.

### Solution: Try Helpers

```ada
with Functional.Try_To_Result;

function Map_Exception (Ex : Exception_Occurrence) return Error is
   ((Unexpected, Exception_Information (Ex)));

function Read_File (Path : String) return String;  -- May raise

package Read_Try is new Functional.Try_To_Result.Instance
   (T              => String,
    E              => Error,
    T_Result       => Str_Result,
    Action         => Read_File,
    Map_Exception  => Map_Exception);

-- Usage
Content : Str_Result.Result := Read_Try.Run ("file.txt");
-- Never raises - always returns Result
```

### Selective Exception Catching

Sometimes you only want to catch specific exceptions:

```ada
with Functional.Catch_Only;

function Is_IO_Error (Ex : Exception_Occurrence) return Boolean is
   (Exception_Identity (Ex) = Ada.IO_Exceptions.Name_Error'Identity);

function Map_IO (Ex : Exception_Occurrence) return Error is
   ((IO_Error, Exception_Message (Ex)));

package Read_IO_Try is new Functional.Catch_Only.Instance
   (T              => String,
    E              => Error,
    T_Result       => Str_Result,
    Action         => Read_File,
    Is_Handled     => Is_IO_Error,
    Map_Exception  => Map_IO);

-- Other exceptions still propagate
```

---

## Railway-Oriented Programming

### Concept

Think of your data flow as a railway with two tracks:
- **Success track** - Operations succeed, value flows forward
- **Failure track** - Error occurs, short-circuits remaining operations

```
Input → Op1 → Op2 → Op3 → Output
        ↓     ↓     ↓
      Error ← Error ← Error
```

### Map vs And_Then

**Map** - Transform success value (stays on success track):
```ada
function Double (X : Integer) return Integer is (X * 2);
function Map_Double is new Int_Result.Map
   (U => Integer, U_Result => Int_Result, F => Double);

R := Map_Double (Int_Result.Ok (21));  -- Ok(42)
R := Map_Double (Int_Result.Err (E));  -- Err(E) - unchanged
```

**And_Then** - Chain fallible operations (can switch to error track):
```ada
function Parse_Int (S : String) return Int_Result.Result is
begin
   return Int_Result.Ok (Integer'Value (S));
exception
   when others =>
      return Int_Result.Err ((Parse_Error, "Invalid integer"));
end Parse_Int;

function Chain_Parse is new Str_Result.And_Then (F => Parse_Int);

R := Chain_Parse (Str_Result.Ok ("42"));    -- Ok(42)
R := Chain_Parse (Str_Result.Ok ("bad"));   -- Err(Parse_Error)
R := Chain_Parse (Str_Result.Err (E));      -- Err(E) - short-circuit
```

### Chaining Example: Complete Pipeline

```ada
-- Read file → Parse JSON → Extract field → Validate

function Read (Path : String) return Str_Result.Result;
function Parse (JSON : String) return Config_Result.Result;
function Extract (C : Config) return Str_Result.Result;
function Validate (S : String) return Str_Result.Result;

-- Chain them
function Parse_Chain is new Str_Result.And_Then (F => Parse);
function Extract_Chain is new Config_Result.And_Then (F => Extract);
function Validate_Chain is new Str_Result.And_Then (F => Validate);

Result := Validate_Chain (Extract_Chain (Parse_Chain (Read ("config.json"))));
-- Stops at first error, returns final success or first failure
```

---

## Advanced Patterns

### Ensure Invariants

```ada
function Is_Positive (X : Integer) return Boolean is (X > 0);
function Positive_Error (X : Integer) return Error is
   ((Validation_Error, "Must be positive, got" & X'Image));

function Ensure_Positive is new Int_Result.Ensure
   (Pred => Is_Positive, To_Error => Positive_Error);

R := Ensure_Positive (Int_Result.Ok (-5));   -- Err(Validation_Error)
R := Ensure_Positive (Int_Result.Ok (10));   -- Ok(10)
```

### Recover from Errors

```ada
-- Recover to a value
function Default_Config (E : Error) return Config is (Empty_Config);
function Recover_Config is new Config_Result.Recover
   (Handle => Default_Config);

Config := Recover_Config (Parse_Config ("missing.toml"));
-- Returns Empty_Config if parse failed

-- Recover to another Result
function Try_Backup (E : Error) return Config_Result.Result is
   (Parse_Config ("backup.toml"));

function Recover_With_Backup is new Config_Result.Recover_With
   (Handle => Try_Backup);

Config := Recover_With_Backup (Parse_Config ("primary.toml"));
-- Tries backup if primary fails
```

### Fallback Chain

```ada
Primary   := Parse_Config ("app.toml");
Secondary := Parse_Config ("default.toml");

-- Eager fallback
Config := Config_Result.Fallback (Primary, Secondary);

-- Lazy fallback (only eval if Primary is Err)
function Default return Config_Result.Result is
   (Config_Result.Ok (Hardcoded_Config));

function Lazy_Fallback is new Config_Result.Fallback_With (F => Default);
Config := Lazy_Fallback (Primary);
```

### Batch Operations

```ada
type Config_Array is array (Positive range <>) of Config;
type Result_Array is array (Positive range <>) of Config_Result.Result;

-- Sequence: All must succeed, or returns first error
function Sequence is new Config_Result.Sequence
   (Ix           => Positive,
    T_Array      => Config_Array,
    Result_Array => Result_Array,
    TA_Result    => Config_Array_Result);

Configs : Result_Array := (Ok(C1), Ok(C2), Err(E), Ok(C3));
All     : Config_Array_Result.Result := Sequence (Configs);
-- Returns Err(E) because one failed
```

### Try + Map Fusion

Catch exceptions AND transform in one step:

```ada
with Functional.Try_Map;

function Risky_Operation return String;  -- May raise
function Len (S : String) return Integer is (S'Length);

package Try_And_Len is new Functional.Try_Map.Instance
   (T              => String,
    U              => Integer,
    E              => Error,
    U_Result       => Int_Result,
    Action         => Risky_Operation,
    Map_Exception  => Map_Ex,
    F              => Len);

Length : Int_Result.Result := Try_And_Len.Run;
-- Catches exceptions, applies Len, returns Result<Integer>
```

---

## Best Practices

### 1. Keep Exceptions at the Edges

```ada
-- ✅ Good: Boundary converts exceptions to Results
function Load_Config (Path : String) return Config_Result.Result is
   package Try is new Try_To_Result.Instance (...);
begin
   return Try.Run (Path);  -- No exceptions escape
end Load_Config;

-- ❌ Bad: Internal function raises
function Load_Config (Path : String) return Config is
begin
   return Parse_File (Path);  -- May raise!
end Load_Config;
```

### 2. Use And_Then for Pipelines

```ada
-- ✅ Good: Clear pipeline
Result := Validate (Extract (Parse (Read (Path))));

-- ❌ Bad: Nested if-then checking
if Is_Ok (R1) then
   R2 := Parse (Value (R1));
   if Is_Ok (R2) then
      R3 := Extract (Value (R2));
      ...
```

### 3. Add Context Early

```ada
-- ✅ Good: Error has full context
Result := With_Context (Parse (Read (Path)),
   "loading user profile from " & Path);

-- ❌ Bad: Error lacks context
Result := Parse (Read (Path));
-- Error just says "parse failed"
```

### 4. Use Type System

```ada
-- ✅ Good: Compiler ensures you handle errors
case Get_User (ID) is
   when Some => Process (Value);
   when None => Use_Default;
end case;

-- ❌ Bad: Runtime failure possible
User := Value (Get_User (ID));  -- Raises if None!
```

---

## Compatibility Aliases

For developers familiar with Rust/Haskell/Scala:

| This Library | Rust | Haskell | Scala |
|--------------|------|---------|-------|
| `And_Then` | `and_then` | `>>=` | `flatMap` |
| `Map` | `map` | `fmap` | `map` |
| `Unwrap_Or` | `unwrap_or` | `fromMaybe` | `getOrElse` |
| `Fallback` | - | `<|>` | `orElse` |
| `With_Context` | `map_err` | - | - |

---

## Summary

**Result<T, E>** - Use for fallible operations (parsing, I/O, validation)
**Option<T>** - Use for absent values (lookups, optional fields)
**Either<L, R>** - Use for neutral disjunctions

**Railway-Oriented Programming** - Chain operations with And_Then, handle errors once at the end

**Exception Boundaries** - Use Try_* helpers to catch at edges, return Results internally

**Next Steps**: See [Generics Design Patterns](generics_design_patterns.md) for when to use generics vs OOP patterns.
