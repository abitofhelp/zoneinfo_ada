pragma Ada_2022;
--  ===========================================================================
--  Zoneinfo.Infrastructure.Query_Timezone_Info
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Query Timezone Info interface and type definitions.
--
--  Key Types:
--    Timezone_Info
--    Query_Result_Type
--
--  Dependencies:
--    ZoneInfo.Domain.Value_Object.Epoch_Seconds
--    ZoneInfo.Domain.Value_Object.UTC_Offset
--    ZoneInfo.Domain.Error.Result
--
--  ===========================================================================

with ZoneInfo.Domain.Value_Object.Epoch_Seconds;
with ZoneInfo.Domain.Value_Object.UTC_Offset;
with ZoneInfo.Domain.Error.Result;

package ZoneInfo.Infrastructure.Query_Timezone_Info is

   use ZoneInfo.Domain.Value_Object.Epoch_Seconds;
   use ZoneInfo.Domain.Value_Object.UTC_Offset;

   --  ========================================================================
   --  Timezone Info Result
   --  ========================================================================

   type Timezone_Info is record
      UTC_Offset   : UTC_Offset_Type;  -- Offset in seconds
      Is_DST       : Boolean;          -- Is daylight saving time active?
      Abbreviation : String (1 .. 10); -- Timezone abbreviation (e.g., "PST")
      Abbr_Length  : Natural;          -- Actual length of abbreviation
   end record;

   --  ========================================================================
   --  Result Type
   --  ========================================================================

   package Info_Result is new
     Domain.Error.Result.Generic_Result (Timezone_Info);
   subtype Query_Result_Type is Info_Result.Result;

   --  ========================================================================
   --  Query_Timezone_Info (Main Public API)
   --  ========================================================================
   --  Query timezone information at a specific epoch timestamp.
   --
   --  Parameters:
   --    Timezone_File : Path to ZoneInfo file (e.g., "/usr/share/zoneinfo/UTC")
   --    Epoch_Time    : Epoch timestamp to query (seconds since 1970-01-01
   --                    UTC)
   --
   --  Returns:
   --    Ok(Timezone_Info) - Information at that time
   --    Error - If file cannot be parsed or time is invalid
   --
   --  Example:
   --    Result := Query_Timezone_Info
   --      ("/var/db/timezone/zoneinfo/America/Los_Angeles", 1733011200);
   --    if Info_Result.Is_Ok (Result) then
   --      Info := Info_Result.Value (Result);
   --      Put_Line ("Offset: " & Info.UTC_Offset'Image);
   --    end if;
   --  ========================================================================

   function Query_Timezone_Info
     (Timezone_File : String; Epoch_Time : Epoch_Seconds_Type)
      return Query_Result_Type;

end ZoneInfo.Infrastructure.Query_Timezone_Info;
