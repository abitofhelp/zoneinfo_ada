pragma Ada_2022;
--  =========================================================================
--  Domain.Value_Object.Civil - Implementation
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--  =========================================================================

with Interfaces;
use Interfaces;
with Domain.Error;
use Domain.Error;

package body Domain.Value_Object.Civil is

   --  ========================================================================
   --  Queries (implemented first as used by Create)
   --  ========================================================================

   function Is_Leap_Year (Year : Year_Number) return Boolean is
   begin
      --  Leap year rules:
      --  1. Divisible by 4
      --  2. But NOT divisible by 100
      --  3. Unless ALSO divisible by 400
      return (Year mod 4 = 0)
             and then ((Year mod 100 /= 0) or else (Year mod 400 = 0));
   end Is_Leap_Year;

   function Days_In_Month
     (Year  : Year_Number;
      Month : Month_Number) return Day_Number
   is
   begin
      case Month is
         when 1 | 3 | 5 | 7 | 8 | 10 | 12 =>
            return 31;
         when 4 | 6 | 9 | 11 =>
            return 30;
         when 2 =>
            if Is_Leap_Year (Year) then
               return 29;
            else
               return 28;
            end if;
      end case;
   end Days_In_Month;

   --  ========================================================================
   --  Smart Constructor
   --  ========================================================================

   function Create
     (Year       : Year_Number;
      Month      : Month_Number;
      Day        : Day_Number;
      Hour       : Hour_Number     := 0;
      Minute     : Minute_Number   := 0;
      Second     : Second_Number   := 0;
      Nanosecond : Nanosecond_Number := 0) return Civil_Result.Result
   is
      Max_Day : Day_Number;
   begin
      --  Validate day for the specific month/year
      Max_Day := Days_In_Month (Year, Month);
      if Day > Max_Day then
         return Civil_Result.Error
           (Kind    => Validation_Error,
            Message => "Day exceeds maximum for given month");
      end if;

      --  All validations passed, create the Civil
      return Civil_Result.Ok
        ((Year       => Year,
          Month      => Month,
          Day        => Day,
          Hour       => Hour,
          Minute     => Minute,
          Second     => Second,
          Nanosecond => Nanosecond));
   end Create;

   --  ========================================================================
   --  Accessors
   --  ========================================================================

   function Get_Year (Self : Civil) return Year_Number is
   begin
      return Self.Year;
   end Get_Year;

   function Get_Month (Self : Civil) return Month_Number is
   begin
      return Self.Month;
   end Get_Month;

   function Get_Day (Self : Civil) return Day_Number is
   begin
      return Self.Day;
   end Get_Day;

   function Get_Hour (Self : Civil) return Hour_Number is
   begin
      return Self.Hour;
   end Get_Hour;

   function Get_Minute (Self : Civil) return Minute_Number is
   begin
      return Self.Minute;
   end Get_Minute;

   function Get_Second (Self : Civil) return Second_Number is
   begin
      return Self.Second;
   end Get_Second;

   function Get_Nanosecond (Self : Civil) return Nanosecond_Number is
   begin
      return Self.Nanosecond;
   end Get_Nanosecond;

   --  ========================================================================
   --  Comparison
   --  ========================================================================

   overriding function "=" (Left, Right : Civil) return Boolean is
   begin
      return Left.Year       = Right.Year
             and then Left.Month      = Right.Month
             and then Left.Day        = Right.Day
             and then Left.Hour       = Right.Hour
             and then Left.Minute     = Right.Minute
             and then Left.Second     = Right.Second
             and then Left.Nanosecond = Right.Nanosecond;
   end "=";

   function "<" (Left, Right : Civil) return Boolean is
   begin
      --  Compare hierarchically: year, month, day, hour, minute, second, nano
      if Left.Year /= Right.Year then
         return Left.Year < Right.Year;
      elsif Left.Month /= Right.Month then
         return Left.Month < Right.Month;
      elsif Left.Day /= Right.Day then
         return Left.Day < Right.Day;
      elsif Left.Hour /= Right.Hour then
         return Left.Hour < Right.Hour;
      elsif Left.Minute /= Right.Minute then
         return Left.Minute < Right.Minute;
      elsif Left.Second /= Right.Second then
         return Left.Second < Right.Second;
      else
         return Left.Nanosecond < Right.Nanosecond;
      end if;
   end "<";

   function "<=" (Left, Right : Civil) return Boolean is
   begin
      return Left < Right or else Left = Right;
   end "<=";

   function ">" (Left, Right : Civil) return Boolean is
   begin
      return Right < Left;
   end ">";

   function ">=" (Left, Right : Civil) return Boolean is
   begin
      return Right <= Left;
   end ">=";

end Domain.Value_Object.Civil;
