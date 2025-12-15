pragma Ada_2022;
--  =========================================================================
--  Domain.Value_Object.Civil - Timezone-blind calendar components
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Defines the Civil value object representing timezone-blind calendar
--    components: year, month, day, hour, minute, second, nanosecond.
--    This is what you see on a wall clock without timezone context.
--
--  Usage:
--    Result := Create (Year   => 2025, Month  => 12, Day    => 3,
--                      Hour   => 14,   Minute => 30, Second => 0,
--                      Nanosecond => 0);
--    if Civil_Result.Is_Ok (Result) then
--       C := Civil_Result.Value (Result);
--       Y := Get_Year (C);
--    end if;
--
--  Design Notes:
--    - Value object pattern: immutable after creation
--    - Validates component ranges at construction
--    - Does NOT validate calendar correctness (e.g., Feb 30)
--      Full calendar validation requires tzif library
--    - Use Zoned.To_Civil for timezone-aware civil times
--    - SPARK-compatible design
--
--  Important:
--    Civil times are ambiguous without timezone context:
--    - "2025-03-09 02:30:00" doesn't exist in America/New_York (DST gap)
--    - "2025-11-02 01:30:00" occurs twice in America/New_York (DST overlap)
--    Converting Civil to Zoned may fail with Gap_Time_Error or
--    Ambiguous_Time_Error.
--
--  See Also:
--    Domain.Value_Object.Zoned - Civil time with timezone context
--  =========================================================================

with Domain.Error.Result;
with Domain.Value_Object.Duration_Type;
use Domain.Value_Object.Duration_Type;

package Domain.Value_Object.Civil
  with Preelaborate
is
   --  ========================================================================
   --  Component Types (Bounded Ranges)
   --  ========================================================================

   --  Year range: practical range for datetime operations
   --  Corresponds roughly to Integer_64 nanosecond range from epoch
   subtype Year_Number is Integer range 1 .. 9999;

   --  Month: 1 = January, 12 = December
   subtype Month_Number is Integer range 1 .. 12;

   --  Day of month: 1..31 (validated at Create for specific month)
   subtype Day_Number is Integer range 1 .. 31;

   --  Hour: 0..23 (24-hour format)
   subtype Hour_Number is Integer range 0 .. 23;

   --  Minute: 0..59
   subtype Minute_Number is Integer range 0 .. 59;

   --  Second: 0..59 (leap seconds not supported)
   subtype Second_Number is Integer range 0 .. 59;

   --  Nanosecond: 0..999_999_999
   subtype Nanosecond_Number is
     Domain.Value_Object.Duration_Type.Nanoseconds_Range;

   --  ========================================================================
   --  Civil Value Object
   --  ========================================================================

   --  Represents timezone-blind calendar components.
   --  This is what appears on a wall clock without knowing the timezone.
   type Civil is record
      Year       : Year_Number;
      Month      : Month_Number;
      Day        : Day_Number;
      Hour       : Hour_Number;
      Minute     : Minute_Number;
      Second     : Second_Number;
      Nanosecond : Nanosecond_Number;
   end record;

   --  ========================================================================
   --  Comparison (must be declared before type freezes)
   --  ========================================================================

   --  Civil times are compared chronologically
   --  Note: This comparison is meaningful only for times in the same timezone
   overriding function "=" (Left, Right : Civil) return Boolean
   with Inline;

   function "<" (Left, Right : Civil) return Boolean;

   function "<=" (Left, Right : Civil) return Boolean
   with Inline;

   function ">" (Left, Right : Civil) return Boolean
   with Inline;

   function ">=" (Left, Right : Civil) return Boolean
   with Inline;

   --  Result type for Civil creation
   package Civil_Result is new
     Domain.Error.Result.Generic_Result (T => Civil);

   --  ========================================================================
   --  Smart Constructor
   --  ========================================================================

   --  Create a Civil datetime from components.
   --  Validates:
   --    - All components in their respective ranges
   --    - Day is valid for the given month/year (including leap years)
   --
   --  Returns: Result[Civil] - Ok if valid, Error if validation fails
   function Create
     (Year       : Year_Number;
      Month      : Month_Number;
      Day        : Day_Number;
      Hour       : Hour_Number     := 0;
      Minute     : Minute_Number   := 0;
      Second     : Second_Number   := 0;
      Nanosecond : Nanosecond_Number := 0) return Civil_Result.Result;

   --  ========================================================================
   --  Accessors
   --  ========================================================================

   function Get_Year (Self : Civil) return Year_Number
   with Inline;

   function Get_Month (Self : Civil) return Month_Number
   with Inline;

   function Get_Day (Self : Civil) return Day_Number
   with Inline;

   function Get_Hour (Self : Civil) return Hour_Number
   with Inline;

   function Get_Minute (Self : Civil) return Minute_Number
   with Inline;

   function Get_Second (Self : Civil) return Second_Number
   with Inline;

   function Get_Nanosecond (Self : Civil) return Nanosecond_Number
   with Inline;

   --  ========================================================================
   --  Queries
   --  ========================================================================

   --  Check if the year is a leap year
   function Is_Leap_Year (Year : Year_Number) return Boolean
   with Inline;

   --  Get the number of days in a given month/year
   function Days_In_Month
     (Year  : Year_Number;
      Month : Month_Number) return Day_Number
   with Inline;

end Domain.Value_Object.Civil;
