pragma Ada_2022;
--  ===========================================================================
--   Timezone Information and Discovery
--  ===========================================================================
--  Demonstrates:
--    - Find_Local_Timezone - Detect system timezone
--    - Get_Timezone_Abbreviation - Get timezone abbreviation (EST, PDT, etc.)
--    - Is_DST_Active - Check if DST is currently active
--  ===========================================================================

with Ada.Text_IO;
with Ada.Calendar;
with ZoneInfo;
with ZoneInfo.API;
with Domain.Value_Object.Zone_Id;
with Domain.Value_Object.Zone_Id.Result;
with Domain.Value_Object.UTC_Offset;
with Domain.Value_Object.UTC_Offset.Result;

procedure Timezone_Info is

   use Ada.Text_IO;

   Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;

   procedure Show_Timezone_Info (Zone : ZoneInfo.Zone_Id; Name : String) is
      --  Get UTC offset
      Offset_Result : constant ZoneInfo.UTC_Offset_Result :=
        ZoneInfo.API.Get_UTC_Offset (Zone, Now);

      --  Get abbreviation
      Abbr : constant String :=
        ZoneInfo.API.Get_Timezone_Abbreviation (Zone, Now);

      --  Check DST status
      DST_Active : constant Boolean :=
        ZoneInfo.API.Is_DST_Active (Zone, Now);
   begin
      Put_Line ("Timezone: " & Name);
      Put_Line ("  Zone ID: " &
        Domain.Value_Object.Zone_Id.To_String (Zone));

      if Domain.Value_Object.UTC_Offset.Result.Is_Ok (Offset_Result) then
         declare
            Offset : constant ZoneInfo.UTC_Offset :=
              Domain.Value_Object.UTC_Offset.Result.Value (Offset_Result);
         begin
            Put_Line ("  UTC Offset: " &
              Domain.Value_Object.UTC_Offset.To_String (Offset));
         end;
      end if;

      Put_Line ("  Abbreviation: " & Abbr);
      Put_Line ("  DST Active: " & (if DST_Active then "Yes" else "No"));
      Put_Line ("");
   end Show_Timezone_Info;

begin
   Put_Line ("========================================================");
   Put_Line ("  ZoneInfo Example: Timezone Information");
   Put_Line ("========================================================");
   Put_Line ("");

   --  Initialize timezone database
   ZoneInfo.API.Initialize;

   --  ========================================================================
   --  Demo 1: Detect Local Timezone
   --  ========================================================================
   Put_Line ("--- Detecting Local System Timezone ---");
   Put_Line ("");

   declare
      Local_Result : constant ZoneInfo.Zone_Id_Result :=
        ZoneInfo.API.Find_Local_Timezone;
   begin
      if Domain.Value_Object.Zone_Id.Result.Is_Ok (Local_Result) then
         declare
            Local_Zone : constant ZoneInfo.Zone_Id :=
              Domain.Value_Object.Zone_Id.Result.Value (Local_Result);
         begin
            Show_Timezone_Info (Local_Zone, "Local System Timezone");
         end;
      else
         Put_Line ("Could not detect local timezone");
         Put_Line ("");
      end if;
   end;

   --  ========================================================================
   --  Demo 2: Show Info for Various Timezones
   --  ========================================================================
   Put_Line ("--- Major World Timezones ---");
   Put_Line ("");

   declare
      type Zone_Info is record
         ID   : access constant String;
         Name : access constant String;
      end record;

      Zones : constant array (Positive range <>) of Zone_Info :=
        ((new String'("America/New_York"), new String'("New York")),
         (new String'("America/Los_Angeles"), new String'("Los Angeles")),
         (new String'("Europe/London"), new String'("London")),
         (new String'("Europe/Paris"), new String'("Paris")),
         (new String'("Asia/Tokyo"), new String'("Tokyo")),
         (new String'("Australia/Sydney"), new String'("Sydney")),
         (new String'("UTC"), new String'("UTC")));
   begin
      for Z of Zones loop
         declare
            Zone_Result : constant ZoneInfo.Zone_Id_Result :=
              Domain.Value_Object.Zone_Id.Result.Create (Z.ID.all);
         begin
            if Domain.Value_Object.Zone_Id.Result.Is_Ok (Zone_Result) then
               declare
                  Zone : constant ZoneInfo.Zone_Id :=
                    Domain.Value_Object.Zone_Id.Result.Value (Zone_Result);
               begin
                  Show_Timezone_Info (Zone, Z.Name.all);
               end;
            end if;
         end;
      end loop;
   end;

   Put_Line ("========================================================");
   Put_Line ("  NOTE: DST status and abbreviations reflect current");
   Put_Line ("  time. They will vary throughout the year as DST");
   Put_Line ("  transitions occur.");
   Put_Line ("========================================================");

end Timezone_Info;
