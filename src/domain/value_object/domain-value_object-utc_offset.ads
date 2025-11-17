pragma Ada_2022;
--  ===========================================================================
--  Domain.Value_Object.Utc_Offset
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Utc Offset value object - immutable domain data.
--
--  Responsibilities:
--    - Define Utc Offset type and operations
--    - Provide constructors and accessors
--
--  Key Types:
--    UTC_Offset_Type
--    for
--    Offset_Seconds_Range
--    UTC_Offset_Type
--
--  Dependencies:
--    Post => To_Seconds'Result in Min_Offset_Seconds .. Max_Offset_Seconds,
--    Post => To_Minutes'Result in (Min_Offset_Seconds / 60) .. (Max_Offset_Seconds / 60),
--    Post => To_String'Result'Length >= 5 and To_String'Result'Length <= 6
--
--  ===========================================================================

package Domain.Value_Object.UTC_Offset is

   pragma Elaborate_Body;

   --  ========================================================================
   --  Constants
   --  ========================================================================

   --  Maximum offset range (in seconds)
   --  Range: ±26 hours to accommodate historical Local Mean Time (LMT) offsets
   --  Modern timezones typically range from -12 to +14 hours, but historical
   --  LMT offsets in IANA tzdata can exceed ±15 hours (e.g., America/Juneau
   --  +15.04h, Asia/Manila -15.94h before standard time zones were adopted)
   Min_Offset_Seconds : constant := -26 * 3_600;  -- -93,600 seconds
   Max_Offset_Seconds : constant :=  26 * 3_600;  --  93,600 seconds

   Seconds_Per_Hour   : constant := 3_600;
   Seconds_Per_Minute : constant := 60;

   --  ========================================================================
   --  UTC_Offset Type
   --  ========================================================================

   --  Private type with range constraint for compile-time validation.
   --  Smart constructors are defined in the child package
   --  Domain.Value_Object.UTC_Offset.Result.
   type UTC_Offset_Type is private;

   --  ========================================================================
   --  Query Operations
   --  ========================================================================

   --  Convert to total seconds
   function To_Seconds (Offset : UTC_Offset_Type) return Integer
   with Post => To_Seconds'Result in Min_Offset_Seconds .. Max_Offset_Seconds,
        Inline;

   --  Convert to minutes (for Ada.Calendar.Time_Zones compatibility)
   function To_Minutes (Offset : UTC_Offset_Type) return Integer
   with Post => To_Minutes'Result in (Min_Offset_Seconds / 60) .. (Max_Offset_Seconds / 60),
        Inline;

   --  Convert to ISO 8601 string format (e.g., "-05:00", "+05:30")
   function To_String (Offset : UTC_Offset_Type) return String
   with Post => To_String'Result'Length >= 5 and To_String'Result'Length <= 6;

   --  Extract components
   function Hours (Offset : UTC_Offset_Type) return Integer
   with Post => Hours'Result in -14 .. 14;

   function Minutes (Offset : UTC_Offset_Type) return Integer
   with Post => Minutes'Result in -59 .. 59;

   --  ========================================================================
   --  Common Offsets (Constants)
   --  ========================================================================

   UTC       : constant UTC_Offset_Type;  --  +00:00
   UTC_Minus_5 : constant UTC_Offset_Type;  --  -05:00 (EST)
   UTC_Minus_8 : constant UTC_Offset_Type;  --  -08:00 (PST)
   UTC_Plus_1  : constant UTC_Offset_Type;  --  +01:00 (CET)
   UTC_Plus_8  : constant UTC_Offset_Type;  --  +08:00 (SGT)

   --  ========================================================================
   --  Operators (FR-11.3: Intuitive usage)
   --  ========================================================================

   --  Equality
   overriding function "=" (Left, Right : UTC_Offset_Type) return Boolean
   with Inline;

   --  Comparison (for sorting)
   function "<" (Left, Right : UTC_Offset_Type) return Boolean
   with Inline;

   --  Negation (flip sign)
   function "-" (Offset : UTC_Offset_Type) return UTC_Offset_Type
   with Inline;

private

   --  ========================================================================
   --  Private Implementation
   --  ========================================================================

   --  Constrained integer subtype for compile-time range validation
   subtype Offset_Seconds_Range is Integer range Min_Offset_Seconds .. Max_Offset_Seconds;

   type UTC_Offset_Type is record
      Seconds : Offset_Seconds_Range := 0;
   end record;

   --  Private unchecked constructor for constants
   function Make_Unchecked (Seconds : Integer) return UTC_Offset_Type is
     (UTC_Offset_Type'(Seconds => Seconds))
   with Pre => Seconds in Min_Offset_Seconds .. Max_Offset_Seconds;

   --  Constants
   UTC         : constant UTC_Offset_Type := Make_Unchecked (0);
   UTC_Minus_5 : constant UTC_Offset_Type := Make_Unchecked (-5 * Seconds_Per_Hour);
   UTC_Minus_8 : constant UTC_Offset_Type := Make_Unchecked (-8 * Seconds_Per_Hour);
   UTC_Plus_1  : constant UTC_Offset_Type := Make_Unchecked (1 * Seconds_Per_Hour);
   UTC_Plus_8  : constant UTC_Offset_Type := Make_Unchecked (8 * Seconds_Per_Hour);

end Domain.Value_Object.UTC_Offset;
