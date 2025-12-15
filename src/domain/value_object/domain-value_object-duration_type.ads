pragma Ada_2022;
--  =========================================================================
--  Domain.Value_Object.Duration_Type - Time span value object
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Defines the Duration_Type value object representing a time span.
--    Duration can be positive or negative, stored as seconds + nanoseconds.
--
--  Usage:
--    D := From_Seconds (3600);           --  1 hour
--    D := From_Millis (1500);            --  1.5 seconds
--    D := From_Nanos (1_000_000_000);    --  1 second
--    Total : Integer_64 := To_Seconds (D);
--    Sum := Add (D1, D2);
--    Neg := Negate (D);
--
--  Design Notes:
--    - Value object pattern: immutable after creation
--    - Seconds can be negative for negative durations
--    - Nanoseconds always in range 0..999_999_999
--    - No heap allocation, SPARK-compatible design
--
--  See Also:
--    Domain.Value_Object.Instant - Uses Duration for arithmetic
--  =========================================================================

with Interfaces;
use Interfaces;

package Domain.Value_Object.Duration_Type
  with Pure
is
   --  ========================================================================
   --  Base Types
   --  ========================================================================

   subtype Integer_64 is Interfaces.Integer_64;

   --  Nanoseconds component is always non-negative (0..999_999_999)
   --  Sign is carried by the Seconds component
   subtype Nanoseconds_Range is Integer_64 range 0 .. 999_999_999;

   Nanos_Per_Second : constant Integer_64 := 1_000_000_000;
   Nanos_Per_Milli  : constant Integer_64 := 1_000_000;
   Nanos_Per_Micro  : constant Integer_64 := 1_000;

   --  ========================================================================
   --  Duration_Type Value Object
   --  ========================================================================

   --  Represents a time span with nanosecond precision.
   --  Seconds can be negative for negative durations.
   --  Nanoseconds is always 0..999_999_999.
   --
   --  For negative durations:
   --    -1.5 seconds = (Seconds => -2, Nanoseconds => 500_000_000)
   --  This ensures Seconds * 10^9 + Nanoseconds gives correct total nanos.
   type Duration_Type is record
      Seconds     : Integer_64;
      Nanoseconds : Nanoseconds_Range;
   end record;

   --  ========================================================================
   --  Comparison (must be declared before type freezes)
   --  ========================================================================

   overriding function "=" (Left, Right : Duration_Type) return Boolean
   with Inline;

   function "<" (Left, Right : Duration_Type) return Boolean
   with Inline;

   function "<=" (Left, Right : Duration_Type) return Boolean
   with Inline;

   function ">" (Left, Right : Duration_Type) return Boolean
   with Inline;

   function ">=" (Left, Right : Duration_Type) return Boolean
   with Inline;

   --  ========================================================================
   --  Constants
   --  ========================================================================

   Zero : constant Duration_Type := (Seconds => 0, Nanoseconds => 0);

   --  ========================================================================
   --  Constructors
   --  ========================================================================

   --  Create duration from total seconds
   function From_Seconds (S : Integer_64) return Duration_Type
   with
     Inline,
     Post => From_Seconds'Result.Nanoseconds = 0;

   --  Create duration from total milliseconds
   function From_Millis (Ms : Integer_64) return Duration_Type
   with Inline;

   --  Create duration from total microseconds
   function From_Micros (Us : Integer_64) return Duration_Type
   with Inline;

   --  Create duration from total nanoseconds
   function From_Nanos (Ns : Integer_64) return Duration_Type
   with Inline;

   --  Create duration from seconds and nanoseconds components
   --  Note: Nanos must be 0..999_999_999; for negative durations,
   --  the sign is carried by Seconds
   function Create
     (Seconds     : Integer_64;
      Nanoseconds : Nanoseconds_Range) return Duration_Type
   with
     Inline,
     Post => Create'Result.Seconds = Seconds
             and then Create'Result.Nanoseconds = Nanoseconds;

   --  ========================================================================
   --  Accessors
   --  ========================================================================

   --  Get the seconds component
   function Get_Seconds (Self : Duration_Type) return Integer_64
   with Inline;

   --  Get the nanoseconds component (always 0..999_999_999)
   function Get_Nanoseconds (Self : Duration_Type) return Nanoseconds_Range
   with Inline;

   --  ========================================================================
   --  Conversions
   --  ========================================================================

   --  Convert to total seconds (truncates nanoseconds)
   function To_Seconds (Self : Duration_Type) return Integer_64
   with Inline;

   --  Convert to total milliseconds (truncates sub-millisecond)
   function To_Millis (Self : Duration_Type) return Integer_64
   with Inline;

   --  Convert to total nanoseconds
   --  Note: May overflow for very large durations
   function To_Nanos (Self : Duration_Type) return Integer_64
   with Inline;

   --  ========================================================================
   --  Arithmetic (Named Functions)
   --  ========================================================================

   --  Add two durations
   function Add (Left, Right : Duration_Type) return Duration_Type
   with Inline;

   --  Subtract Right from Left
   function Subtract (Left, Right : Duration_Type) return Duration_Type
   with Inline;

   --  Negate a duration
   function Negate (Self : Duration_Type) return Duration_Type
   with Inline;

   --  ========================================================================
   --  Arithmetic Operators
   --  ========================================================================
   --
   --  Duration operators return Duration_Type directly (not Result) because:
   --  1. Integer_64 seconds can represent ~292 billion years - overflow is
   --     practically impossible for duration arithmetic
   --  2. Keeps Duration_Type as a Pure package
   --  3. Duration is used internally; Instant operators (which return Result)
   --     are the main user-facing operations
   --
   --  ========================================================================

   --  Add two durations: Duration + Duration -> Duration
   function "+" (Left, Right : Duration_Type) return Duration_Type
   with Inline;

   --  Subtract durations: Duration - Duration -> Duration
   function "-" (Left, Right : Duration_Type) return Duration_Type
   with Inline;

   --  Negate a duration: -Duration -> Duration
   function "-" (Right : Duration_Type) return Duration_Type
   with Inline;

   --  ========================================================================
   --  Queries
   --  ========================================================================

   --  Check if duration is negative
   function Is_Negative (Self : Duration_Type) return Boolean
   with Inline;

   --  Check if duration is zero
   function Is_Zero (Self : Duration_Type) return Boolean
   with
     Inline,
     Post => Is_Zero'Result = (Self = Zero);

end Domain.Value_Object.Duration_Type;
