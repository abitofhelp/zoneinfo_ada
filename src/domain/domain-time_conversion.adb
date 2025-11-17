pragma Ada_2022;
--  ===========================================================================
--  Domain.Time_Conversion
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Time Conversion implementation.
--
--  ===========================================================================

with Ada.Calendar.Arithmetic;

package body Domain.Time_Conversion is

   use Ada.Calendar;
   use Ada.Calendar.Arithmetic;
   use Domain.Error;

   --  ========================================================================
   --  Conversion Functions
   --  ========================================================================

   function To_Epoch_Seconds (T : Ada.Calendar.Time) return Epoch_Result.Result is

      Days    : Day_Count;
      Seconds : Duration;
      Leap_Seconds : Leap_Seconds_Count;
   begin
      --  Check if time is in valid range
      if not Is_Valid_For_Unix_Epoch (T) then
         return Epoch_Result.Error
           (Kind    => Time_Out_Of_Range,
            Message => "Time is outside valid Unix epoch range (1901-2099)");
      end if;

      --  Calculate difference from Unix epoch
      --  Ada.Calendar.Arithmetic.Difference returns:
      --    - Days: number of days difference
      --    - Seconds: fractional day in seconds (0.0 .. 86400.0)
      --    - Leap_Seconds: leap second count (0 when -y flag not used)
      Difference (T, Unix_Epoch_Time, Days, Seconds, Leap_Seconds);

      --  Convert to total seconds
      --  Note: We ignore Leap_Seconds because:
      --    1. IANA uses POSIX time (no leap seconds)
      --    2. Ada defaults to POSIX mode (no -y binder flag)
      --    3. If -y is used, this creates incompatibility (documented in SRS)
      declare
         Day_Seconds  : constant Long_Long_Integer :=
           Long_Long_Integer (Days) * 86_400;
         Frac_Seconds : constant Long_Long_Integer :=
           Long_Long_Integer (Seconds);
         Total_Seconds : constant Long_Long_Integer :=
           Day_Seconds + Frac_Seconds;
      begin
         return Ok (From_Seconds (Total_Seconds));
      end;

   exception
      when Constraint_Error | Time_Error =>
         return Epoch_Result.Error
           (Kind    => Conversion_Error,
            Message => "Failed to convert Ada.Calendar.Time to Unix epoch");
   end To_Epoch_Seconds;

   function To_Ada_Time (Epoch : Epoch_Seconds_Type) return Time_Result.Result is

      Total_Seconds : constant Long_Long_Integer := To_Seconds (Epoch);
      Days          : constant Day_Count :=
        Day_Count (Total_Seconds / 86_400);
      Seconds       : constant Duration :=
        Duration (Total_Seconds rem 86_400);
      Result_Time   : Ada.Calendar.Time;
   begin
      --  Check if epoch is in valid Ada.Calendar.Time range
      if not Is_Valid_For_Ada_Time (Epoch) then
         return Time_Result.Error
           (Kind    => Time_Out_Of_Range,
            Message => "Unix epoch" & Total_Seconds'Image &
                      " is outside Ada.Calendar.Time range (1901-2099)");
      end if;

      --  Add days and seconds to Unix epoch
      Result_Time := Unix_Epoch_Time + (Duration (Days) * 86_400.0) + Seconds;

      return Ok (Result_Time);

   exception
      when Constraint_Error | Time_Error =>
         return Time_Result.Error
           (Kind    => Conversion_Error,
            Message => "Failed to convert Unix epoch to Ada.Calendar.Time");
   end To_Ada_Time;

   --  ========================================================================
   --  Convenience Functions
   --  ========================================================================

   function To_Epoch_Seconds_Unchecked (T : Ada.Calendar.Time)
                                        return Epoch_Seconds_Type is
      Result : constant Epoch_Result.Result := To_Epoch_Seconds (T);
   begin
      return Epoch_Result.Value (Result);
   end To_Epoch_Seconds_Unchecked;

   function To_Ada_Time_Unchecked (Epoch : Epoch_Seconds_Type)
                                   return Ada.Calendar.Time is
      Result : constant Time_Result.Result := To_Ada_Time (Epoch);
   begin
      return Time_Result.Value (Result);
   end To_Ada_Time_Unchecked;

   --  ========================================================================
   --  Query Functions
   --  ========================================================================

   function Is_Valid_For_Unix_Epoch (T : Ada.Calendar.Time) return Boolean is
   begin
      --  Ada.Calendar.Time range is 1901-2099 (GNAT)
      --  This is well within Unix epoch range (-2^63 to 2^63-1)
      --  So all Ada times are valid for Unix epoch
      return T >= Min_Ada_Time and T <= Max_Ada_Time;
   end Is_Valid_For_Unix_Epoch;

   function Is_Valid_For_Ada_Time (Epoch : Epoch_Seconds_Type) return Boolean is
      --  Approximate seconds for year 1901 and 2099
      --  1901-01-01: approximately -2177452800 seconds from 1970
      --  2099-12-31: approximately  4102444799 seconds from 1970
      Min_Epoch : constant Long_Long_Integer := -2_177_452_800;
      Max_Epoch : constant Long_Long_Integer :=  4_102_444_799;
      Seconds   : constant Long_Long_Integer := To_Seconds (Epoch);
   begin
      return Seconds >= Min_Epoch and Seconds <= Max_Epoch;
   end Is_Valid_For_Ada_Time;

end Domain.Time_Conversion;
