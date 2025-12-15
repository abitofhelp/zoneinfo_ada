pragma Ada_2022;
--  ===========================================================================
--  Zoneinfo.API.Operations - Pure Operations (SPARK-Safe)
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Pure datetime operations that don't require clock or timezone ports.
--    These operations work directly on domain types without I/O.
--    SPARK-Mode enabled for formal verification.
--
--  Architecture:
--    - Pure computation only (no I/O)
--    - Depends ONLY on Domain types
--    - SPARK-safe: all operations formally verifiable
--
--  Usage:
--    with Zoneinfo.API;
--    with Zoneinfo.API.Operations;
--    use Zoneinfo.API;
--
--    --  Add duration to instant
--    New_Instant := Operations.Add (Some_Instant, Some_Duration);
--
--    --  Calculate difference
--    Elapsed := Operations.Diff (End_Time, Start_Time);
--
--  ===========================================================================

pragma SPARK_Mode (On);

with Domain.Value_Object.Instant;
with Domain.Value_Object.Duration_Type;

package Zoneinfo.API.Operations
  with Preelaborate
is

   --  Import types for use in operations
   subtype Instant is Domain.Value_Object.Instant.Instant;
   subtype Duration_Type is Domain.Value_Object.Duration_Type.Duration_Type;
   package Instant_Result renames Domain.Value_Object.Instant.Instant_Result;

   --  ========================================================================
   --  Instant Arithmetic (Named Functions)
   --  ========================================================================

   --  Add a duration to an instant
   function Add
     (I : Instant;
      D : Duration_Type) return Instant_Result.Result
   renames Domain.Value_Object.Instant.Add;

   --  Subtract a duration from an instant
   function Subtract
     (I : Instant;
      D : Duration_Type) return Instant_Result.Result
   renames Domain.Value_Object.Instant.Subtract;

   --  Calculate duration between two instants (Other - Self)
   function Diff
     (Self  : Instant;
      Other : Instant) return Duration_Type
   renames Domain.Value_Object.Instant.Diff;

   --  ========================================================================
   --  Instant Arithmetic Operators
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
   renames Domain.Value_Object.Instant."+";

   --  Subtract duration from instant: Instant - Duration -> Result[Instant]
   function "-" (Left : Instant; Right : Duration_Type)
     return Instant_Result.Result
   renames Domain.Value_Object.Instant."-";

   --  Calculate difference: Instant - Instant -> Duration
   function "-" (Left, Right : Instant) return Duration_Type
   renames Domain.Value_Object.Instant."-";

   --  ========================================================================
   --  Duration Arithmetic (Named Functions)
   --  ========================================================================

   --  Add two durations
   function Add
     (Left, Right : Duration_Type) return Duration_Type
   renames Domain.Value_Object.Duration_Type.Add;

   --  Subtract two durations
   function Subtract
     (Left, Right : Duration_Type) return Duration_Type
   renames Domain.Value_Object.Duration_Type.Subtract;

   --  Negate a duration
   function Negate (D : Duration_Type) return Duration_Type
   renames Domain.Value_Object.Duration_Type.Negate;

   --  ========================================================================
   --  Duration Arithmetic Operators
   --  ========================================================================

   --  Add two durations: Duration + Duration -> Duration
   function "+" (Left, Right : Duration_Type) return Duration_Type
   renames Domain.Value_Object.Duration_Type."+";

   --  Subtract two durations: Duration - Duration -> Duration
   function "-" (Left, Right : Duration_Type) return Duration_Type
   renames Domain.Value_Object.Duration_Type."-";

   --  Negate a duration: -Duration -> Duration
   function "-" (Right : Duration_Type) return Duration_Type
   renames Domain.Value_Object.Duration_Type."-";

end Zoneinfo.API.Operations;
