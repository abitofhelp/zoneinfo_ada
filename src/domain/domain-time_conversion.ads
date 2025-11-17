pragma Ada_2022;
--  ===========================================================================
--  Domain.Time_Conversion
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Time Conversion interface and type definitions.
--
--  Dependencies:
--    Post => (if Epoch_Result.Is_Ok (To_Epoch_Seconds'Result)
--    Post => (if Time_Result.Is_Ok (To_Ada_Time'Result)
--    Pre => (declare
--
--  ===========================================================================

with Ada.Calendar;
with Domain.Value_Object.Epoch_Seconds;
with Domain.Error;
with Domain.Error.Result;

package Domain.Time_Conversion is

   pragma Elaborate_Body;

   use Domain.Value_Object.Epoch_Seconds;

   --  ========================================================================
   --  Result Types
   --  ========================================================================

   package Time_Result is new Domain.Error.Result.Generic_Result
     (T => Ada.Calendar.Time);
   use Time_Result;

   package Epoch_Result is new Domain.Error.Result.Generic_Result
     (T => Epoch_Seconds_Type);
   use Epoch_Result;

   --  ========================================================================
   --  Conversion Functions
   --  ========================================================================

   --  Convert Ada.Calendar.Time to Unix epoch seconds
   --  Returns Error if time is outside valid Unix epoch range
   function To_Epoch_Seconds (T : Ada.Calendar.Time) return Epoch_Result.Result
   with Post => (if Epoch_Result.Is_Ok (To_Epoch_Seconds'Result)
                 then abs To_Seconds (Epoch_Result.Value (To_Epoch_Seconds'Result)) < 2**62);

   --  Convert Unix epoch seconds to Ada.Calendar.Time
   --  Returns Error if epoch is outside Ada.Calendar.Time range (1901-2099)
   function To_Ada_Time (Epoch : Epoch_Seconds_Type) return Time_Result.Result
   with Post => (if Time_Result.Is_Ok (To_Ada_Time'Result)
                 then To_Ada_Time'Result = To_Ada_Time'Result);  -- Identity check

   --  ========================================================================
   --  Convenience Functions (Unwrap for Known-Valid Values)
   --  ========================================================================

   --  Convert to epoch, raising Constraint_Error if out of range
   --  ONLY use when you know the time is valid (e.g., Clock result)
   --  NOT for user input!
   function To_Epoch_Seconds_Unchecked (T : Ada.Calendar.Time) return Epoch_Seconds_Type
   with Pre => (declare
                   R : constant Epoch_Result.Result := To_Epoch_Seconds (T);
                begin
                   Epoch_Result.Is_Ok (R));

   --  Convert to Ada time, raising Constraint_Error if out of range
   --  ONLY use when you know the epoch is valid
   --  NOT for user input!
   function To_Ada_Time_Unchecked (Epoch : Epoch_Seconds_Type) return Ada.Calendar.Time
   with Pre => (declare
                   R : constant Time_Result.Result := To_Ada_Time (Epoch);
                begin
                   Time_Result.Is_Ok (R));

   --  ========================================================================
   --  Query Functions
   --  ========================================================================

   --  Check if Ada.Calendar.Time is within valid Unix epoch range
   function Is_Valid_For_Unix_Epoch (T : Ada.Calendar.Time) return Boolean;

   --  Check if Unix epoch is within Ada.Calendar.Time range
   function Is_Valid_For_Ada_Time (Epoch : Epoch_Seconds_Type) return Boolean;

   --  ========================================================================
   --  Constants
   --  ========================================================================

   --  Unix epoch as Ada.Calendar.Time: 1970-01-01 00:00:00
   Unix_Epoch_Time : constant Ada.Calendar.Time;

   --  Min/max representable dates in Ada.Calendar (GNAT-specific)
   --  GNAT: 1901-01-01 to 2099-12-31
   Min_Ada_Time : constant Ada.Calendar.Time;
   Max_Ada_Time : constant Ada.Calendar.Time;

private

   --  ========================================================================
   --  Private Constants
   --  ========================================================================

   --  Precomputed Unix epoch time (1970-01-01 00:00:00)
   Unix_Epoch_Time : constant Ada.Calendar.Time :=
     Ada.Calendar.Time_Of (Year => 1970, Month => 1, Day => 1, Seconds => 0.0);

   --  Min/max Ada.Calendar.Time values (GNAT range: 1901-2099)
   Min_Ada_Time : constant Ada.Calendar.Time :=
     Ada.Calendar.Time_Of (Year => 1901, Month => 1, Day => 1, Seconds => 0.0);

   Max_Ada_Time : constant Ada.Calendar.Time :=
     Ada.Calendar.Time_Of (Year => 2099, Month => 12, Day => 31, Seconds => 86_399.999_999_999);

end Domain.Time_Conversion;
