pragma Ada_2022;
--  =========================================================================
--  Infrastructure.Adapter.Tzif - Implementation
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Uses tzif library for full timezone support including DST handling.
--  Note: Uses Zoneinfo.TZif_Lib alias to avoid name shadowing with this
--  package (Infrastructure.Adapter.Tzif shadows the library name TZif).
--  =========================================================================

with Interfaces;
use Interfaces;

--  Use the library alias to avoid name collision with this package
with Zoneinfo.TZif_Lib.API;
with Zoneinfo.TZif_Lib.Domain.Value_Object.Transition_Info;

package body Infrastructure.Adapter.Tzif is

   --  Local package renames for brevity
   package TZif_Api renames Zoneinfo.TZif_Lib.API;
   package TZif_Trans renames
     Zoneinfo.TZif_Lib.Domain.Value_Object.Transition_Info;

   Nanos_Per_Second : constant Integer_64 := 1_000_000_000;
   Seconds_Per_Day  : constant Integer_64 := 86_400;

   --  Days from year 1 to year Y (not counting Y itself)
   function Days_Before_Year (Y : Integer) return Integer_64 is
      Years : constant Integer_64 := Integer_64 (Y - 1);
   begin
      return Years * 365 + Years / 4 - Years / 100 + Years / 400;
   end Days_Before_Year;

   --  Days in months before month M in year Y
   function Days_Before_Month (Y : Integer; M : Integer) return Integer is
      Days : constant array (1 .. 12) of Integer :=
        [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334];
      Leap_Adjust : Integer := 0;
      Is_Leap     : Boolean;
   begin
      Is_Leap :=
        (Y mod 4 = 0 and then Y mod 100 /= 0) or else Y mod 400 = 0;
      if M > 2 and then Is_Leap then
         Leap_Adjust := 1;
      end if;
      return Days (M) + Leap_Adjust;
   end Days_Before_Month;

   --  Unix epoch reference: 1970-01-01
   Days_Before_1970 : constant Integer_64 := Days_Before_Year (1970);

   --  Convert epoch nanoseconds to Civil time (helper for UTC)
   function Epoch_Nanos_To_Civil
     (Epoch_Nanos : Integer_64) return Civil
   is
      Total_Seconds     : Integer_64;
      Remaining_Nanos   : Integer_64;
      Days_Since_Epoch  : Integer_64;
      Time_Of_Day       : Integer_64;
      Year_Val          : Integer;
      Month_Val         : Integer;
      Day_Val           : Integer;
      Hour_Val          : Integer;
      Minute_Val        : Integer;
      Second_Val        : Integer;
      Days_In_Month_Val : Integer;
      Remaining_Days    : Integer_64;
      Civil_Result      : Domain.Value_Object.Civil.Civil_Result.Result;
   begin
      --  Split into seconds and nanoseconds
      if Epoch_Nanos >= 0 then
         Total_Seconds := Epoch_Nanos / Nanos_Per_Second;
         Remaining_Nanos := Epoch_Nanos mod Nanos_Per_Second;
      else
         Total_Seconds :=
           (Epoch_Nanos - Nanos_Per_Second + 1) / Nanos_Per_Second;
         Remaining_Nanos := Epoch_Nanos - Total_Seconds * Nanos_Per_Second;
      end if;

      --  Split into days and time-of-day
      if Total_Seconds >= 0 then
         Days_Since_Epoch := Total_Seconds / Seconds_Per_Day;
         Time_Of_Day := Total_Seconds mod Seconds_Per_Day;
      else
         Days_Since_Epoch :=
           (Total_Seconds - Seconds_Per_Day + 1) / Seconds_Per_Day;
         Time_Of_Day := Total_Seconds - Days_Since_Epoch * Seconds_Per_Day;
      end if;

      --  Calculate time components
      Hour_Val := Integer (Time_Of_Day / 3600);
      Minute_Val := Integer ((Time_Of_Day mod 3600) / 60);
      Second_Val := Integer (Time_Of_Day mod 60);

      --  Calculate date from days since epoch
      Remaining_Days := Days_Since_Epoch + Days_Before_1970;

      --  Find year using 366 as divisor to get a low estimate
      Year_Val := Integer (Remaining_Days / 366);
      if Year_Val < 1 then
         Year_Val := 1;
      end if;

      --  Increment while next year's start is still <= our day count
      while Days_Before_Year (Year_Val + 1) <= Remaining_Days and then
            Year_Val < 9999
      loop
         Year_Val := Year_Val + 1;
      end loop;

      Remaining_Days := Remaining_Days - Days_Before_Year (Year_Val);

      --  Find month
      Month_Val := 1;
      while Month_Val < 12 loop
         Days_In_Month_Val :=
           Days_Before_Month (Year_Val, Month_Val + 1) -
           Days_Before_Month (Year_Val, Month_Val);
         if Remaining_Days >= Integer_64 (Days_In_Month_Val) then
            Remaining_Days :=
              Remaining_Days - Integer_64 (Days_In_Month_Val);
            Month_Val := Month_Val + 1;
         else
            exit;
         end if;
      end loop;

      --  Ensure Day_Val is in valid range (1..31)
      Day_Val := Integer (Remaining_Days) + 1;
      if Day_Val < 1 then
         Day_Val := 1;
      elsif Day_Val > 31 then
         Day_Val := 31;
      end if;

      --  Create Civil (this should not fail with valid epoch values)
      Civil_Result :=
        Domain.Value_Object.Civil.Create
          (Year       => Year_Val,
           Month      => Month_Val,
           Day        => Day_Val,
           Hour       => Hour_Val,
           Minute     => Minute_Val,
           Second     => Second_Val,
           Nanosecond =>
             Domain.Value_Object.Duration_Type.Nanoseconds_Range
               (Remaining_Nanos));

      --  If creation fails (shouldn't happen), return a safe default
      if Domain.Value_Object.Civil.Civil_Result.Is_Error (Civil_Result) then
         return
           (Year       => 1970,
            Month      => 1,
            Day        => 1,
            Hour       => 0,
            Minute     => 0,
            Second     => 0,
            Nanosecond => 0);
      end if;

      return Domain.Value_Object.Civil.Civil_Result.Value (Civil_Result);
   end Epoch_Nanos_To_Civil;

   --  ========================================================================
   --  Timezone_Port Implementation
   --  ========================================================================

   function To_Civil (I : Instant; Zone : Zone_ID) return Civil is
      Epoch_Nanos   : constant Integer_64 :=
        Domain.Value_Object.Instant.Get_Epoch_Nanos (I);
      Epoch_Secs    : constant Integer_64 := Epoch_Nanos / Nanos_Per_Second;

      Zone_Str     : constant String :=
        Domain.Value_Object.Zone_ID.To_String (Zone);
      Zone_Id_Bnd  : constant TZif_Api.Zone_Id_String :=
        TZif_Api.Make_Zone_Id_String (Zone_Str);
      Epoch_Sec    : constant TZif_Api.Epoch_Seconds_Type :=
        TZif_Api.Epoch_Seconds_Type (Epoch_Secs);

      Trans_Result : constant TZif_Api.Transition_Result :=
        TZif_Api.Get_Transition_At_Epoch (Zone_Id_Bnd, Epoch_Sec);

      Offset_Seconds    : Integer_64 := 0;
      Local_Epoch_Nanos : Integer_64;
   begin
      --  Get offset from tzif if zone is valid
      if TZif_Api.Is_Ok (Trans_Result) then
         declare
            Info : constant TZif_Trans.Transition_Info_Type :=
              TZif_Api.Get_Transition_Port.Get_Transition_Result_Package.Value
                (Trans_Result);
         begin
            Offset_Seconds :=
              Integer_64 (TZif_Trans.Get_UTC_Offset_Seconds (Info));
         end;
      end if;

      --  Apply offset: local_time = utc_time + offset
      Local_Epoch_Nanos := Epoch_Nanos + Offset_Seconds * Nanos_Per_Second;

      return Epoch_Nanos_To_Civil (Local_Epoch_Nanos);
   end To_Civil;

   function To_Instant
     (C : Civil; Zone : Zone_ID) return Instant_Result.Result
   is
      use Domain.Value_Object.Civil;

      Year_Val  : constant Integer := Get_Year (C);
      Month_Val : constant Integer := Get_Month (C);
      Day_Val   : constant Integer := Get_Day (C);

      Days_Total    : Integer_64;
      Seconds_Total : Integer_64;
      Nanos_Total   : Integer_64;

      Zone_Str    : constant String :=
        Domain.Value_Object.Zone_ID.To_String (Zone);
      Zone_Id_Bnd : constant TZif_Api.Zone_Id_String :=
        TZif_Api.Make_Zone_Id_String (Zone_Str);

      Offset_Seconds : Integer_64 := 0;
   begin
      --  Calculate days from epoch (as if UTC)
      Days_Total :=
        Days_Before_Year (Year_Val) +
        Integer_64 (Days_Before_Month (Year_Val, Month_Val)) +
        Integer_64 (Day_Val - 1) - Days_Before_1970;

      --  Calculate seconds
      Seconds_Total :=
        Days_Total * Seconds_Per_Day + Integer_64 (Get_Hour (C)) * 3600 +
        Integer_64 (Get_Minute (C)) * 60 + Integer_64 (Get_Second (C));

      --  Get UTC offset from tzif
      declare
         Epoch_Sec    : constant TZif_Api.Epoch_Seconds_Type :=
           TZif_Api.Epoch_Seconds_Type (Seconds_Total);
         Trans_Result : constant TZif_Api.Transition_Result :=
           TZif_Api.Get_Transition_At_Epoch (Zone_Id_Bnd, Epoch_Sec);
      begin
         if TZif_Api.Is_Ok (Trans_Result) then
            declare
               Info : constant TZif_Trans.Transition_Info_Type :=
                 TZif_Api.Get_Transition_Port.Get_Transition_Result_Package
                   .Value (Trans_Result);
            begin
               Offset_Seconds :=
                 Integer_64 (TZif_Trans.Get_UTC_Offset_Seconds (Info));
            end;
         end if;
      end;

      --  Apply offset: utc_time = local_time - offset
      Seconds_Total := Seconds_Total - Offset_Seconds;

      --  Add nanoseconds
      Nanos_Total :=
        Seconds_Total * Nanos_Per_Second + Integer_64 (Get_Nanosecond (C));

      return
        Instant_Result.Ok
          (Domain.Value_Object.Instant.From_Epoch_Nanos (Nanos_Total));
   end To_Instant;

   function Is_Valid_Zone (Zone : Zone_ID) return Boolean is
      Zone_Str : constant String :=
        Domain.Value_Object.Zone_ID.To_String (Zone);

      --  Try to create a TZif Zone_Id using the smart constructor
      --  (returns Result since tzif v3.0.0)
      Zone_Id_Res : constant TZif_Api.Zone_Id_Result :=
        TZif_Api.Make_Zone_Id (Zone_Str);
   begin
      --  If Zone_Id creation failed, zone string is invalid
      if not TZif_Api.Is_Ok (Zone_Id_Res) then
         return False;
      end if;

      --  Try to find the zone in the tzdb
      declare
         Zone_Id_Val : constant TZif_Api.Zone_Id_Type :=
           TZif_Api.Value (Zone_Id_Res);
         Find_Result : constant TZif_Api.Zone_Result :=
           TZif_Api.Find_By_Id (Zone_Id_Val);
      begin
         return TZif_Api.Is_Ok (Find_Result);
      end;
   end Is_Valid_Zone;

   function Get_UTC_Offset (I : Instant; Zone : Zone_ID) return Duration_Type
   is
      Epoch_Nanos   : constant Integer_64 :=
        Domain.Value_Object.Instant.Get_Epoch_Nanos (I);
      Epoch_Secs    : constant Integer_64 := Epoch_Nanos / Nanos_Per_Second;

      Zone_Str    : constant String :=
        Domain.Value_Object.Zone_ID.To_String (Zone);
      Zone_Id_Bnd : constant TZif_Api.Zone_Id_String :=
        TZif_Api.Make_Zone_Id_String (Zone_Str);
      Epoch_Sec   : constant TZif_Api.Epoch_Seconds_Type :=
        TZif_Api.Epoch_Seconds_Type (Epoch_Secs);

      Trans_Result : constant TZif_Api.Transition_Result :=
        TZif_Api.Get_Transition_At_Epoch (Zone_Id_Bnd, Epoch_Sec);
   begin
      if TZif_Api.Is_Ok (Trans_Result) then
         declare
            Info        : constant TZif_Trans.Transition_Info_Type :=
              TZif_Api.Get_Transition_Port.Get_Transition_Result_Package.Value
                (Trans_Result);
            Offset_Secs : constant Integer :=
              TZif_Trans.Get_UTC_Offset_Seconds (Info);
         begin
            return
              Domain.Value_Object.Duration_Type.From_Seconds
                (Interfaces.Integer_64 (Offset_Secs));
         end;
      else
         --  Zone not found or error - return zero offset (UTC)
         return Domain.Value_Object.Duration_Type.Zero;
      end if;
   end Get_UTC_Offset;

end Infrastructure.Adapter.Tzif;
