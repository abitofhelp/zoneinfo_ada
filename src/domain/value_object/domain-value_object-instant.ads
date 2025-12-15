pragma Ada_2022;
--  =========================================================================
--  Domain.Value_Object.Instant - Absolute moment in time
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Defines the Instant value object representing an absolute moment in
--    time, stored as nanoseconds since Unix epoch (1970-01-01 00:00:00 UTC).
--    This is a timezone-agnostic point on the timeline.
--
--  Usage:
--    --  Create from Unix epoch components
--    Result := From_Unix_Epoch (Seconds => 1700000000, Nanos => 500_000_000);
--
--    --  Arithmetic with Duration
--    Later := Add (Now, Duration_Type.From_Seconds (3600));
--    Elapsed := Diff (End_Time, Start_Time);
--
--    --  Convert back to epoch components
--    Epoch := To_Unix_Epoch (I);  --  Returns (Seconds, Nanos)
--
--  Design Notes:
--    - Value object pattern: immutable after creation
--    - Internally stored as nanoseconds since Unix epoch (Integer_64)
--    - Timezone-agnostic: represents a single point on the timeline
--    - Use Zoned when timezone context is needed
--    - SPARK-compatible design
--
--  Range:
--    Integer_64 nanoseconds can represent approximately:
--    -292 years to +292 years from epoch (sufficient for practical use)
--
--  See Also:
--    Domain.Value_Object.Zoned - Instant with timezone context
--    Domain.Value_Object.Duration_Type - Time span for arithmetic
--  =========================================================================

with Interfaces;
with Domain.Value_Object.Duration_Type;
with Domain.Error.Result;

package Domain.Value_Object.Instant
  with Preelaborate
is
   --  ========================================================================
   --  Base Types
   --  ========================================================================

   subtype Integer_64 is Interfaces.Integer_64;

   --  Re-export Duration_Type for convenience
   subtype Duration_Type is Domain.Value_Object.Duration_Type.Duration_Type;

   --  Nanoseconds component range (same as Duration_Type)
   subtype Nanoseconds_Range is
     Domain.Value_Object.Duration_Type.Nanoseconds_Range;

   --  ========================================================================
   --  Instant Value Object
   --  ========================================================================

   --  Represents an absolute moment in time as nanoseconds since Unix epoch.
   --  This is the canonical internal representation.
   type Instant is record
      Epoch_Nanos : Integer_64;
   end record;

   --  ========================================================================
   --  Comparison (must be declared before type freezes)
   --  ========================================================================

   overriding function "=" (Left, Right : Instant) return Boolean
   with Inline;

   function "<" (Left, Right : Instant) return Boolean
   with Inline;

   function "<=" (Left, Right : Instant) return Boolean
   with Inline;

   function ">" (Left, Right : Instant) return Boolean
   with Inline;

   function ">=" (Left, Right : Instant) return Boolean
   with Inline;

   --  Result type for Instant creation
   package Instant_Result is new
     Domain.Error.Result.Generic_Result (T => Instant);

   --  ========================================================================
   --  Constants
   --  ========================================================================

   --  Unix epoch: 1970-01-01 00:00:00 UTC
   Epoch : constant Instant := (Epoch_Nanos => 0);

   --  ========================================================================
   --  Constructors
   --  ========================================================================

   --  Create Instant from Unix epoch seconds and nanoseconds
   --  Nanos must be in range 0..999_999_999
   --  Returns Overflow_Error if seconds * 1_000_000_000 + nanos would overflow
   --  (approximately ±292 years from epoch is the valid range)
   function From_Unix_Epoch
     (Seconds : Integer_64;
      Nanos   : Nanoseconds_Range := 0) return Instant_Result.Result;

   --  Create Instant from total nanoseconds since epoch
   function From_Epoch_Nanos (Nanos : Integer_64) return Instant
   with Inline;

   --  ========================================================================
   --  Epoch Record for Output
   --  ========================================================================

   --  Structured representation of Unix epoch time
   type Unix_Epoch_Type is record
      Seconds     : Integer_64;
      Nanoseconds : Nanoseconds_Range;
   end record;

   --  ========================================================================
   --  Accessors
   --  ========================================================================

   --  Get the raw nanoseconds since epoch
   function Get_Epoch_Nanos (Self : Instant) return Integer_64
   with Inline;

   --  Convert to Unix epoch components
   function To_Unix_Epoch (Self : Instant) return Unix_Epoch_Type
   with Inline;

   --  ========================================================================
   --  Arithmetic with Duration (Named Functions)
   --  ========================================================================

   --  Add a duration to an instant
   function Add
     (Self : Instant;
      D    : Duration_Type) return Instant_Result.Result;

   --  Subtract a duration from an instant
   function Subtract
     (Self : Instant;
      D    : Duration_Type) return Instant_Result.Result;

   --  Calculate the duration between two instants (Other - Self)
   function Diff
     (Self  : Instant;
      Other : Instant) return Duration_Type
   with Inline;

   --  ========================================================================
   --  Arithmetic Operators
   --  ========================================================================
   --
   --  Operators return Result types for functional error handling.
   --  This is non-idiomatic Ada but aligns with the railway-oriented design.
   --
   --  Usage:
   --    Result := Some_Instant + Some_Duration;
   --    if Instant_Result.Is_Ok (Result) then ...
   --
   --  ========================================================================

   --  Add duration to instant: Instant + Duration -> Result[Instant]
   function "+" (Left : Instant; Right : Duration_Type)
     return Instant_Result.Result
   with Inline;

   --  Subtract duration from instant: Instant - Duration -> Result[Instant]
   function "-" (Left : Instant; Right : Duration_Type)
     return Instant_Result.Result
   with Inline;

   --  Calculate difference: Instant - Instant -> Duration
   --  Note: This always succeeds, so returns Duration directly
   function "-" (Left, Right : Instant) return Duration_Type
   with Inline;

end Domain.Value_Object.Instant;
