pragma Ada_2022;
--  ===========================================================================
--  Zoneinfo
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Zoneinfo interface and type definitions.
--
--  Key Types:
--    Zone_Id
--    Epoch_Seconds
--    UTC_Offset
--    Time
--    Disambiguation_Strategy
--    ... and 8 more
--
--  ===========================================================================

with Ada.Calendar;
with Domain.Value_Object.Zone_Id;
with Domain.Value_Object.Zone_Id.Result;
with Domain.Value_Object.Epoch_Seconds;
with Domain.Value_Object.UTC_Offset;
with Domain.Value_Object.UTC_Offset.Result;
with Domain.Value_Object.DST_Info;
with Domain.Value_Object.DST_Info.Result;
with Domain.Time_Conversion;
with Domain.Error;

package ZoneInfo is

   pragma Elaborate_Body;

   --  Library version
   Version : constant String := "0.1.0";

   --  ========================================================================
   --  Re-export Domain Types for Convenience
   --  ========================================================================

   --  Timezone identifier (e.g., "America/New_York")
   subtype Zone_Id is Domain.Value_Object.Zone_Id.Zone_Id_Type;

   --  Unix epoch seconds (seconds since 1970-01-01 00:00:00 UTC)
   subtype Epoch_Seconds is Domain.Value_Object.Epoch_Seconds.Epoch_Seconds_Type;

   --  UTC offset in seconds (range: -14h to +14h)
   subtype UTC_Offset is Domain.Value_Object.UTC_Offset.UTC_Offset_Type;

   --  Standard Ada time type
   subtype Time is Ada.Calendar.Time;

   --  DST disambiguation strategy (for ambiguous times)
   subtype Disambiguation_Strategy is Domain.Value_Object.DST_Info.Disambiguation_Strategy;

   --  DST ambiguity information
   subtype Ambiguity_Info is Domain.Value_Object.DST_Info.Ambiguity_Info_Type;

   --  ========================================================================
   --  Re-export Result Types
   --  ========================================================================

   --  Result types for value object construction
   subtype Zone_Id_Result is Domain.Value_Object.Zone_Id.Result.Result;
   subtype UTC_Offset_Result is Domain.Value_Object.UTC_Offset.Result.Result;
   subtype Time_Result is Domain.Time_Conversion.Time_Result.Result;
   subtype Epoch_Result is Domain.Time_Conversion.Epoch_Result.Result;
   subtype Ambiguity_Info_Result is Domain.Value_Object.DST_Info.Result.Result;

   --  Error type for all operations
   subtype Error_Type is Domain.Error.Error_Type;
   subtype Error_Kind is Domain.Error.Error_Kind;

   --  ========================================================================
   --  Re-export Smart Constructors
   --  ========================================================================

   --  Zone_Id constructors
   function Create_Zone_Id (ID : String) return Zone_Id_Result
     renames Domain.Value_Object.Zone_Id.Result.Create;

   --  UTC_Offset constructors
   function Create_UTC_Offset_From_Seconds (Seconds : Integer) return UTC_Offset_Result
     renames Domain.Value_Object.UTC_Offset.Result.From_Seconds;

   function Create_UTC_Offset_From_Hours_Minutes
     (Hours : Integer; Minutes : Integer := 0) return UTC_Offset_Result
     renames Domain.Value_Object.UTC_Offset.Result.From_Hours_Minutes;

   function Create_UTC_Offset_From_String (Offset_Str : String) return UTC_Offset_Result
     renames Domain.Value_Object.UTC_Offset.Result.From_String;

   --  Epoch_Seconds constructors
   function Create_Epoch_Seconds (Seconds : Long_Long_Integer) return Epoch_Seconds
     renames Domain.Value_Object.Epoch_Seconds.From_Seconds;

   --  Time conversion
   function To_Epoch_Seconds (T : Time) return Epoch_Result
     renames Domain.Time_Conversion.To_Epoch_Seconds;

   function To_Ada_Time (Epoch : Epoch_Seconds) return Time_Result
     renames Domain.Time_Conversion.To_Ada_Time;

   --  ========================================================================
   --  Re-export Query Operations
   --  ========================================================================

   --  Note: Query operations are available directly from Domain packages.
   --  Users can call:
   --    Domain.Value_Object.Zone_Id.To_String
   --    Domain.Value_Object.UTC_Offset.To_String
   --    Domain.Value_Object.UTC_Offset.To_Seconds
   --    Domain.Value_Object.Epoch_Seconds.To_Seconds

   --  ========================================================================
   --  Re-export Common Constants
   --  ========================================================================

   --  Common timezone identifiers
   UTC_Zone : Zone_Id renames Domain.Value_Object.Zone_Id.UTC;

   --  Common UTC offsets
   UTC_Offset_Zero : UTC_Offset renames Domain.Value_Object.UTC_Offset.UTC;

   --  Unix epoch time
   Unix_Epoch_Time : Time renames Domain.Time_Conversion.Unix_Epoch_Time;

   --  Disambiguation strategy constants
   Prefer_Earlier : constant Disambiguation_Strategy :=
     Domain.Value_Object.DST_Info.Prefer_Earlier;
   Prefer_Later : constant Disambiguation_Strategy :=
     Domain.Value_Object.DST_Info.Prefer_Later;
   Raise_On_Ambiguous : constant Disambiguation_Strategy :=
     Domain.Value_Object.DST_Info.Raise_On_Ambiguous;

end ZoneInfo;
