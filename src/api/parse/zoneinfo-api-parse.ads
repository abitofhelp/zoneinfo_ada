pragma Ada_2022;
--  ===========================================================================
--  Zoneinfo.API.Parse - Datetime String Parsing
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Pure string parsing operations for datetime types. Converts ISO 8601
--    string representations to domain types.
--
--  Architecture:
--    - Pure string operations (no I/O, no TZif dependency)
--    - SPARK-Mode Off: Uses string parsing that may be complex for SPARK
--    - Returns Result types for parse error handling
--
--  Usage:
--    with Zoneinfo.API.Parse;
--
--    --  Parse ISO 8601 datetime
--    Result := Parse.From_ISO_8601 ("2025-12-04T14:30:00");
--    if Civil_Result.Is_Ok (Result) then
--       Civil_Time := Civil_Result.Value (Result);
--    end if;
--
--    --  Parse with offset
--    Result := Parse.From_ISO_8601_With_Offset ("2025-12-04T14:30:00-05:00");
--
--  ===========================================================================

pragma SPARK_Mode (Off);

with Domain.Value_Object.Civil;
with Domain.Value_Object.Duration_Type;
with Domain.Value_Object.Zone_ID;
with Domain.Error.Result;

package Zoneinfo.API.Parse
  with Preelaborate
is

   --  ========================================================================
   --  Type Re-exports
   --  ========================================================================

   subtype Civil is Domain.Value_Object.Civil.Civil;
   subtype Duration_Type is Domain.Value_Object.Duration_Type.Duration_Type;
   subtype Zone_ID is Domain.Value_Object.Zone_ID.Zone_ID;

   package Civil_Result renames Domain.Value_Object.Civil.Civil_Result;
   package Zone_ID_Result renames Domain.Value_Object.Zone_ID.Zone_ID_Result;

   --  ========================================================================
   --  Result Types for Parse Operations
   --  ========================================================================

   --  Parse result containing Civil + optional offset
   type Civil_With_Offset is record
      Time   : Civil;
      Offset : Duration_Type;
      Has_Offset : Boolean;
   end record;

   package Civil_Offset_Result is new
     Domain.Error.Result.Generic_Result (T => Civil_With_Offset);

   --  Parse result containing Civil + Zone_ID
   type Civil_With_Zone is record
      Time : Civil;
      Zone : Zone_ID;
   end record;

   package Civil_Zone_Result is new
     Domain.Error.Result.Generic_Result (T => Civil_With_Zone);

   --  Full parse result with offset and zone
   type Civil_Full is record
      Time       : Civil;
      Offset     : Duration_Type;
      Zone       : Zone_ID;
      Has_Offset : Boolean;
      Has_Zone   : Boolean;
   end record;

   package Civil_Full_Result is new
     Domain.Error.Result.Generic_Result (T => Civil_Full);

   --  Duration parse result
   package Duration_Result is new
     Domain.Error.Result.Generic_Result (T => Duration_Type);

   --  ========================================================================
   --  Civil Parsing
   --  ========================================================================

   --  Parse ISO 8601 date-time string to Civil
   --  Accepts: "2025-12-04T14:30:00" or "2025-12-04T14:30:00.123456789"
   --
   --  Returns: Result[Civil] - Ok if valid, Parse_Error if invalid
   function From_ISO_8601 (S : String) return Civil_Result.Result;

   --  Parse ISO 8601 with UTC offset
   --  Accepts: "2025-12-04T14:30:00-05:00" or "2025-12-04T14:30:00Z"
   --
   --  Returns: Result[Civil_With_Offset] containing civil time and offset
   function From_ISO_8601_With_Offset
     (S : String) return Civil_Offset_Result.Result;

   --  Parse ISO 8601 with zone ID
   --  Accepts: "2025-12-04T14:30:00[America/New_York]"
   --
   --  Returns: Result[Civil_With_Zone] containing civil time and zone
   function From_ISO_8601_With_Zone
     (S : String) return Civil_Zone_Result.Result;

   --  Parse full ISO 8601 with offset and/or zone
   --  Accepts: "2025-12-04T14:30:00-05:00[America/New_York]"
   --           "2025-12-04T14:30:00Z"
   --           "2025-12-04T14:30:00[UTC]"
   --           "2025-12-04T14:30:00"
   --
   --  Returns: Result[Civil_Full] with all parsed components
   function From_ISO_8601_Full (S : String) return Civil_Full_Result.Result;

   --  ========================================================================
   --  Date-Only Parsing
   --  ========================================================================

   --  Parse ISO date string
   --  Accepts: "2025-12-04"
   function From_ISO_Date (S : String) return Civil_Result.Result;

   --  ========================================================================
   --  Time-Only Parsing
   --  ========================================================================

   --  Parse ISO time string
   --  Accepts: "14:30:00" or "14:30:00.123456789"
   --  Note: Returns Civil with date set to 1970-01-01
   function From_ISO_Time (S : String) return Civil_Result.Result;

   --  ========================================================================
   --  Duration Parsing
   --  ========================================================================

   --  Parse ISO 8601 duration string
   --  Accepts: "PT1H30M45S", "P1DT12H", "-PT5M", "PT0S"
   --
   --  ISO 8601 duration format: PnDTnHnMnS
   --  - P = period designator (required)
   --  - nD = days
   --  - T = time designator (required if H/M/S present)
   --  - nH = hours
   --  - nM = minutes
   --  - nS = seconds (may include decimal)
   function From_ISO_Duration (S : String) return Duration_Result.Result;

   --  Parse human-readable duration string
   --  Accepts: "1h 30m 45s", "2d 12h", "-5m 30s", "0s"
   function From_Human_Duration (S : String) return Duration_Result.Result;

   --  ========================================================================
   --  Offset Parsing
   --  ========================================================================

   --  Parse UTC offset string
   --  Accepts: "+05:00", "-05:00", "Z", "+0530"
   function Parse_Offset (S : String) return Duration_Result.Result;

end Zoneinfo.API.Parse;
