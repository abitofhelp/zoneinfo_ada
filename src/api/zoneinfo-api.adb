pragma Ada_2022;
--  ===========================================================================
--  Zoneinfo.Api
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Api implementation.
--
--  ===========================================================================

with Ada.Calendar;
with Domain.Value_Object.Zone_Id;
with Domain.Value_Object.UTC_Offset.Result;
with Domain.Value_Object.DST_Info.Result;
with Domain.Time_Conversion;
with Domain.Error;

package body ZoneInfo.API is

   use Ada.Calendar;
   use Domain.Error;

   --  TEMPORARY STUB: All operations return errors until TZif dependency is restored
   Stub_Error_Message : constant String :=
     "ZoneInfo requires tzif dependency (pending Alire approval)";

   Initialized : Boolean := False;

   --  ========================================================================
   --  Stub Implementations - Return Errors
   --  ========================================================================

   function Get_UTC_Offset
     (Zone : ZoneInfo.Zone_Id;
      T    : ZoneInfo.Time)
      return ZoneInfo.UTC_Offset_Result
   is
      pragma Unreferenced (Zone, T);
   begin
      return Domain.Value_Object.UTC_Offset.Result.Error
        (Kind    => Infrastructure_Error,
         Message => Stub_Error_Message);
   end Get_UTC_Offset;

   function Get_UTC_Offset_With_Disambiguation
     (Zone     : ZoneInfo.Zone_Id;
      T        : ZoneInfo.Time;
      Strategy : ZoneInfo.Disambiguation_Strategy := ZoneInfo.Prefer_Earlier)
      return ZoneInfo.UTC_Offset_Result
   is
      pragma Unreferenced (Zone, T, Strategy);
   begin
      return Domain.Value_Object.UTC_Offset.Result.Error
        (Kind    => Infrastructure_Error,
         Message => Stub_Error_Message);
   end Get_UTC_Offset_With_Disambiguation;

   function Get_Ambiguity_Info
     (Zone : ZoneInfo.Zone_Id;
      T    : ZoneInfo.Time)
      return ZoneInfo.Ambiguity_Info_Result
   is
      pragma Unreferenced (Zone, T);
   begin
      return Domain.Value_Object.DST_Info.Result.Error
        (Kind    => Infrastructure_Error,
         Message => Stub_Error_Message);
   end Get_Ambiguity_Info;

   function Convert_Time
     (T         : ZoneInfo.Time;
      From_Zone : ZoneInfo.Zone_Id;
      To_Zone   : ZoneInfo.Zone_Id)
      return ZoneInfo.Time_Result
   is
      pragma Unreferenced (T, From_Zone, To_Zone);
   begin
      return Domain.Time_Conversion.Time_Result.Error
        (Kind    => Infrastructure_Error,
         Message => Stub_Error_Message);
   end Convert_Time;

   function Is_Ambiguous_Time
     (Zone : ZoneInfo.Zone_Id;
      T    : ZoneInfo.Time)
      return Boolean
   is
      pragma Unreferenced (Zone, T);
   begin
      return False;
   end Is_Ambiguous_Time;

   function Is_Gap_Time
     (Zone : ZoneInfo.Zone_Id;
      T    : ZoneInfo.Time)
      return Boolean
   is
      pragma Unreferenced (Zone, T);
   begin
      return False;
   end Is_Gap_Time;

   function Find_Local_Timezone return ZoneInfo.Zone_Id_Result is
   begin
      return Domain.Value_Object.Zone_Id.Result.Error
        (Kind    => Infrastructure_Error,
         Message => Stub_Error_Message);
   end Find_Local_Timezone;

   function Get_Timezone_Abbreviation
     (Zone : ZoneInfo.Zone_Id;
      T    : ZoneInfo.Time)
      return String
   is
      pragma Unreferenced (Zone, T);
   begin
      return "STUB";
   end Get_Timezone_Abbreviation;

   function Is_DST_Active
     (Zone : ZoneInfo.Zone_Id;
      T    : ZoneInfo.Time)
      return Boolean
   is
      pragma Unreferenced (Zone, T);
   begin
      return False;
   end Is_DST_Active;

   function Get_Timezone_Count return Natural is
   begin
      return 0;
   end Get_Timezone_Count;

   function List_All_Timezones
     (Descending : Boolean := False)
      return Zone_Array
   is
      pragma Unreferenced (Descending);
   begin
      return Zone_Array'(1 .. 0 => <>);
   end List_All_Timezones;

   function Find_Timezones_By_Pattern
     (Pattern : String)
      return Zone_Array
   is
      pragma Unreferenced (Pattern);
   begin
      return Zone_Array'(1 .. 0 => <>);
   end Find_Timezones_By_Pattern;

   function Find_Timezones_By_Region
     (Region : String)
      return Zone_Array
   is
      pragma Unreferenced (Region);
   begin
      return Zone_Array'(1 .. 0 => <>);
   end Find_Timezones_By_Region;

   procedure Initialize is
   begin
      Initialized := True;
   end Initialize;

   function Is_Initialized return Boolean is
   begin
      return Initialized;
   end Is_Initialized;

end ZoneInfo.API;
