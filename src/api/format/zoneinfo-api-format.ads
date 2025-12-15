pragma Ada_2022;
--  ===========================================================================
--  Zoneinfo.API.Format - Datetime String Formatting
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Pure string formatting operations for datetime types. Converts domain
--    types to ISO 8601 string representations.
--
--  Architecture:
--    - Pure string operations (no I/O, no TZif dependency)
--    - SPARK-Mode On: formally verifiable
--    - Works on Civil/Duration that caller obtained via API.Desktop
--
--  Usage:
--    with Zoneinfo.API.Format;
--    with Zoneinfo.API.Desktop;
--
--    --  Get civil time via Desktop (uses TZif internally)
--    Civil_Time : constant Civil := To_Civil (Some_Zoned);
--    Offset     : constant Duration_Type := Get_Offset (Some_Zoned);
--
--    --  Format to ISO 8601 string
--    S := Format.To_ISO_8601 (Civil_Time);
--    --  Result: "2025-12-04T14:30:00"
--
--    S := Format.To_ISO_8601 (Civil_Time, Offset);
--    --  Result: "2025-12-04T14:30:00-05:00"
--
--    S := Format.To_ISO_8601 (Civil_Time, Zone_ID);
--    --  Result: "2025-12-04T14:30:00[America/New_York]"
--
--  ===========================================================================

pragma SPARK_Mode (On);

with Ada.Strings.Bounded;
with Domain.Value_Object.Civil;
with Domain.Value_Object.Duration_Type;
with Domain.Value_Object.Zone_ID;
with Domain.Value_Object.Instant;

package Zoneinfo.API.Format
  with Preelaborate
is

   --  ========================================================================
   --  Type Re-exports
   --  ========================================================================

   subtype Civil is Domain.Value_Object.Civil.Civil;
   subtype Duration_Type is Domain.Value_Object.Duration_Type.Duration_Type;
   subtype Zone_ID is Domain.Value_Object.Zone_ID.Zone_ID;
   subtype Instant is Domain.Value_Object.Instant.Instant;

   --  ========================================================================
   --  Output String Types
   --  ========================================================================

   --  Maximum length for formatted datetime strings
   --  ISO 8601: "2025-12-04T14:30:00.123456789-12:00
   --            [America/Argentina/Buenos_Aires]" = ~80 chars max
   Max_Datetime_Length : constant := 96;

   package Datetime_Strings is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max => Max_Datetime_Length);
   subtype Datetime_String is Datetime_Strings.Bounded_String;

   --  Maximum length for formatted duration strings
   --  ISO 8601 Duration: "P999999999DT23H59M59.999999999S" = ~40 chars
   Max_Duration_Length : constant := 48;

   package Duration_Strings is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max => Max_Duration_Length);
   subtype Duration_String is Duration_Strings.Bounded_String;

   --  ========================================================================
   --  Civil Formatting
   --  ========================================================================

   --  Format Civil to ISO 8601 date-time (no timezone info)
   --  Result: "2025-12-04T14:30:00" or "2025-12-04T14:30:00.123456789"
   --
   --  Parameters:
   --    C             : Civil datetime to format
   --    Include_Nanos : Include nanoseconds if non-zero (default: True)
   --
   --  Note: Use this for timezone-blind civil times
   function To_ISO_8601
     (C             : Civil;
      Include_Nanos : Boolean := True) return Datetime_String;

   --  Format Civil with UTC offset
   --  Result: "2025-12-04T14:30:00-05:00" or "2025-12-04T14:30:00Z"
   --
   --  Parameters:
   --    C             : Civil datetime to format
   --    Offset        : UTC offset (from Get_Offset)
   --    Include_Nanos : Include nanoseconds if non-zero (default: True)
   --
   --  Note: Offset of zero produces "Z" suffix (UTC)
   function To_ISO_8601_With_Offset
     (C             : Civil;
      Offset        : Duration_Type;
      Include_Nanos : Boolean := True) return Datetime_String;

   --  Format Civil with zone ID (IANA name)
   --  Result: "2025-12-04T14:30:00[America/New_York]"
   --
   --  Parameters:
   --    C             : Civil datetime to format
   --    Zone          : Zone ID to append
   --    Include_Nanos : Include nanoseconds if non-zero (default: True)
   function To_ISO_8601_With_Zone
     (C             : Civil;
      Zone          : Zone_ID;
      Include_Nanos : Boolean := True) return Datetime_String;

   --  Format Civil with both offset and zone ID
   --  Result: "2025-12-04T14:30:00-05:00[America/New_York]"
   --
   --  This is the most complete format, showing both offset and zone name
   function To_ISO_8601_Full
     (C             : Civil;
      Offset        : Duration_Type;
      Zone          : Zone_ID;
      Include_Nanos : Boolean := True) return Datetime_String;

   --  ========================================================================
   --  Date-Only Formatting
   --  ========================================================================

   --  Format date portion only
   --  Result: "2025-12-04"
   function To_ISO_Date (C : Civil) return Datetime_String;

   --  ========================================================================
   --  Time-Only Formatting
   --  ========================================================================

   --  Format time portion only
   --  Result: "14:30:00" or "14:30:00.123456789"
   function To_ISO_Time
     (C             : Civil;
      Include_Nanos : Boolean := True) return Datetime_String;

   --  ========================================================================
   --  Instant Formatting
   --  ========================================================================

   --  Format Instant as epoch seconds (machine-readable)
   --  Result: "1733329800" or "1733329800.123456789"
   function To_Epoch_String
     (I             : Instant;
      Include_Nanos : Boolean := True) return Datetime_String;

   --  ========================================================================
   --  Duration Formatting
   --  ========================================================================

   --  Format Duration to ISO 8601 duration
   --  Result: "PT1H30M45S" or "P1DT12H" or "-PT5M" (negative)
   --
   --  ISO 8601 duration format: PnDTnHnMnS
   --  - P = period designator (required)
   --  - nD = days
   --  - T = time designator (required if H/M/S present)
   --  - nH = hours
   --  - nM = minutes
   --  - nS = seconds (may include decimal for nanoseconds)
   function To_ISO_Duration (D : Duration_Type) return Duration_String;

   --  Format Duration as human-readable string
   --  Result: "1h 30m 45s" or "2d 12h" or "-5m 30s"
   function To_Human_Duration (D : Duration_Type) return Duration_String;

   --  ========================================================================
   --  Helper Functions
   --  ========================================================================

   --  Format UTC offset as "+HH:MM" or "-HH:MM" or "Z"
   function Format_Offset (Offset : Duration_Type) return Datetime_String;

   --  ========================================================================
   --  String Conversion Convenience Functions
   --  ========================================================================
   --
   --  These are renames of the bounded string To_String functions for
   --  convenience in non-SPARK desktop code. SPARK code should use the
   --  bounded string types directly.

   --  Convert Datetime_String to String
   function To_String (S : Datetime_String) return String
     renames Datetime_Strings.To_String;

   --  Convert Duration_String to String
   function To_String (S : Duration_String) return String
     renames Duration_Strings.To_String;

end Zoneinfo.API.Format;
