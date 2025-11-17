pragma Ada_2022;
-- ===========================================================================
--  Timezone Conversion Example - Practical Scenarios
-- ===========================================================================
--  Demonstrates real-world timezone conversion scenarios:
--    - Global team coordination
--    - International conference calls
--    - Trading hours across financial markets
--    - Server timestamp correlation
-- ===========================================================================

with Ada.Text_IO;
with Ada.Calendar;
with Ada.Strings.Bounded;
with Ada.Strings.Fixed;
with ZoneInfo;
with ZoneInfo.API;
with Domain.Value_Object.Zone_Id;
with Domain.Value_Object.Zone_Id.Result;
with Domain.Value_Object.UTC_Offset;
with Domain.Value_Object.UTC_Offset.Result;

procedure Timezone_Conversion is

   use Ada.Text_IO;
   use type Ada.Calendar.Time;

   package Zone_Names is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 64);
   use Zone_Names;

   type Timezone_Info is record
      Name : Bounded_String;
      Zone_Id : ZoneInfo.Zone_Id;
      Offset : Duration;
   end record;

   procedure Print_Time_In_Timezone
     (Label : String;
      UTC_Time : Ada.Calendar.Time;
      TZ : Timezone_Info);

   procedure Print_Time_In_Timezone
     (Label : String;
      UTC_Time : Ada.Calendar.Time;
      TZ : Timezone_Info)
   is
      use Ada.Calendar;
      Local_Time : constant Ada.Calendar.Time := UTC_Time + TZ.Offset;
      Year   : Year_Number;
      Month  : Month_Number;
      Day    : Day_Number;
      Seconds : Day_Duration;
      Hour   : Integer;
      Minute : Integer;
      Second : Integer;
      Remainder : Day_Duration;
   begin
      Split (Local_Time, Year, Month, Day, Seconds);
      Hour := Integer (Seconds) / 3600;
      Remainder := Seconds - Day_Duration (Hour * 3600);
      Minute := Integer (Remainder) / 60;
      Second := Integer (Remainder) mod 60;

      Put (Label & " - " & To_String (TZ.Name) & ": ");
      Put (Year'Image & "-");
      if Month < 10 then Put ("0"); end if;
      Put (Ada.Strings.Fixed.Trim (Month'Image, Ada.Strings.Left) & "-");
      if Day < 10 then Put ("0"); end if;
      Put (Ada.Strings.Fixed.Trim (Day'Image, Ada.Strings.Left) & " ");
      if Hour < 10 then Put ("0"); end if;
      Put (Ada.Strings.Fixed.Trim (Hour'Image, Ada.Strings.Left) & ":");
      if Minute < 10 then Put ("0"); end if;
      Put_Line (Ada.Strings.Fixed.Trim (Minute'Image, Ada.Strings.Left));
   end Print_Time_In_Timezone;

   procedure Example_1_Global_Standup;
   procedure Example_2_Market_Hours;
   procedure Example_3_Conference_Call;

   procedure Example_1_Global_Standup is
      --  Daily standup at 9 AM San Francisco time
      SF_Zone_Result : constant ZoneInfo.Zone_Id_Result :=
        Domain.Value_Object.Zone_Id.Result.Create ("America/Los_Angeles");

      London_Zone_Result : constant ZoneInfo.Zone_Id_Result :=
        Domain.Value_Object.Zone_Id.Result.Create ("Europe/London");

      Bangalore_Zone_Result : constant ZoneInfo.Zone_Id_Result :=
        Domain.Value_Object.Zone_Id.Result.Create ("Asia/Kolkata");

      Standup_Time_SF : constant Ada.Calendar.Time :=
        Ada.Calendar.Time_Of (2024, 11, 20, 9.0 * 3600.0);  -- 9 AM
   begin
      Put_Line ("=== Example 1: Global Team Daily Standup ===");
      Put_Line ("");
      Put_Line ("Engineering team has daily standup at 9 AM San Francisco time.");
      Put_Line ("What time should team members in London and Bangalore join?");
      Put_Line ("");

      if Domain.Value_Object.Zone_Id.Result.Is_Ok (SF_Zone_Result) and then
         Domain.Value_Object.Zone_Id.Result.Is_Ok (London_Zone_Result) and then
         Domain.Value_Object.Zone_Id.Result.Is_Ok (Bangalore_Zone_Result)
      then
         declare
            SF_Zone : constant ZoneInfo.Zone_Id :=
              Domain.Value_Object.Zone_Id.Result.Value (SF_Zone_Result);

            London_Zone : constant ZoneInfo.Zone_Id :=
              Domain.Value_Object.Zone_Id.Result.Value (London_Zone_Result);

            Bangalore_Zone : constant ZoneInfo.Zone_Id :=
              Domain.Value_Object.Zone_Id.Result.Value (Bangalore_Zone_Result);

            SF_Offset_Result : constant ZoneInfo.UTC_Offset_Result :=
              ZoneInfo.API.Get_UTC_Offset (SF_Zone, Standup_Time_SF);

            London_Offset_Result : constant ZoneInfo.UTC_Offset_Result :=
              ZoneInfo.API.Get_UTC_Offset (London_Zone, Standup_Time_SF);

            Bangalore_Offset_Result : constant ZoneInfo.UTC_Offset_Result :=
              ZoneInfo.API.Get_UTC_Offset (Bangalore_Zone, Standup_Time_SF);
         begin
            if Domain.Value_Object.UTC_Offset.Result.Is_Ok (SF_Offset_Result) and then
               Domain.Value_Object.UTC_Offset.Result.Is_Ok (London_Offset_Result) and then
               Domain.Value_Object.UTC_Offset.Result.Is_Ok (Bangalore_Offset_Result)
            then
               declare
                  SF_Offset : constant Duration :=
                    Duration (Domain.Value_Object.UTC_Offset.To_Seconds
                      (Domain.Value_Object.UTC_Offset.Result.Value (SF_Offset_Result)));

                  London_Offset : constant Duration :=
                    Duration (Domain.Value_Object.UTC_Offset.To_Seconds
                      (Domain.Value_Object.UTC_Offset.Result.Value (London_Offset_Result)));

                  Bangalore_Offset : constant Duration :=
                    Duration (Domain.Value_Object.UTC_Offset.To_Seconds
                      (Domain.Value_Object.UTC_Offset.Result.Value (Bangalore_Offset_Result)));

                  Standup_UTC : constant Ada.Calendar.Time := Standup_Time_SF - SF_Offset;

                  SF_TZ : constant Timezone_Info :=
                    (Name => To_Bounded_String ("San Francisco"),
                     Zone_Id => SF_Zone,
                     Offset => SF_Offset);

                  London_TZ : constant Timezone_Info :=
                    (Name => To_Bounded_String ("London"),
                     Zone_Id => London_Zone,
                     Offset => London_Offset);

                  Bangalore_TZ : constant Timezone_Info :=
                    (Name => To_Bounded_String ("Bangalore"),
                     Zone_Id => Bangalore_Zone,
                     Offset => Bangalore_Offset);
               begin
                  Print_Time_In_Timezone ("Standup", Standup_UTC, SF_TZ);
                  Print_Time_In_Timezone ("Standup", Standup_UTC, London_TZ);
                  Print_Time_In_Timezone ("Standup", Standup_UTC, Bangalore_TZ);
                  Put_Line ("");
                  Put_Line ("Result: 9 AM SF = 5 PM London = 10:30 PM Bangalore");
               end;
            end if;
         end;
      end if;
      Put_Line ("");
   end Example_1_Global_Standup;

   procedure Example_2_Market_Hours is
      NYSE_Zone_Result : constant ZoneInfo.Zone_Id_Result :=
        Domain.Value_Object.Zone_Id.Result.Create ("America/New_York");

      LSE_Zone_Result : constant ZoneInfo.Zone_Id_Result :=
        Domain.Value_Object.Zone_Id.Result.Create ("Europe/London");

      Tokyo_Zone_Result : constant ZoneInfo.Zone_Id_Result :=
        Domain.Value_Object.Zone_Id.Result.Create ("Asia/Tokyo");

      Trading_Day : constant Ada.Calendar.Time :=
        Ada.Calendar.Time_Of (2024, 11, 20, 0.0);  -- Reference date
   begin
      Put_Line ("=== Example 2: Global Financial Market Hours ===");
      Put_Line ("");
      Put_Line ("When do major stock exchanges overlap?");
      Put_Line ("");

      if Domain.Value_Object.Zone_Id.Result.Is_Ok (NYSE_Zone_Result) and then
         Domain.Value_Object.Zone_Id.Result.Is_Ok (LSE_Zone_Result) and then
         Domain.Value_Object.Zone_Id.Result.Is_Ok (Tokyo_Zone_Result)
      then
         declare
            NYSE_Open_Local : constant Ada.Calendar.Time :=
              Ada.Calendar.Time_Of (2024, 11, 20, 9.5 * 3600.0);  -- 9:30 AM

            NYSE_Close_Local : constant Ada.Calendar.Time :=
              Ada.Calendar.Time_Of (2024, 11, 20, 16.0 * 3600.0); -- 4:00 PM

            NYSE_Zone : constant ZoneInfo.Zone_Id :=
              Domain.Value_Object.Zone_Id.Result.Value (NYSE_Zone_Result);

            NYSE_Offset_Result : constant ZoneInfo.UTC_Offset_Result :=
              ZoneInfo.API.Get_UTC_Offset (NYSE_Zone, Trading_Day);
         begin
            if Domain.Value_Object.UTC_Offset.Result.Is_Ok (NYSE_Offset_Result) then
               declare
                  NYSE_Offset : constant Duration :=
                    Duration (Domain.Value_Object.UTC_Offset.To_Seconds
                      (Domain.Value_Object.UTC_Offset.Result.Value (NYSE_Offset_Result)));

                  NYSE_Open_UTC : constant Ada.Calendar.Time := NYSE_Open_Local - NYSE_Offset;
                  NYSE_Close_UTC : constant Ada.Calendar.Time := NYSE_Close_Local - NYSE_Offset;
               begin
                  Put_Line ("NYSE (New York Stock Exchange):");
                  Put_Line ("  Opens:  9:30 AM EST");
                  Put_Line ("  Closes: 4:00 PM EST");
                  Put_Line ("");
                  Put_Line ("Trading hours: 6.5 hours (9:30 AM - 4:00 PM)");
                  Put_Line ("Lunch: Continuous trading (no break)");
               end;
            end if;
         end;
      end if;
      Put_Line ("");
   end Example_2_Market_Hours;

   procedure Example_3_Conference_Call is
      type String_Access is access constant String;
      Participants : constant array (Positive range <>) of String_Access :=
        [new String'("America/Los_Angeles"),
         new String'("America/Chicago"),
         new String'("America/New_York"),
         new String'("Europe/London"),
         new String'("Europe/Berlin"),
         new String'("Asia/Tokyo")];

      --  Find best time: 2 PM UTC
      Reference_UTC : constant Ada.Calendar.Time :=
        Ada.Calendar.Time_Of (2024, 11, 20, 14.0 * 3600.0);
   begin
      Put_Line ("=== Example 3: International Conference Call ===");
      Put_Line ("");
      Put_Line ("Finding a good time for global participants:");
      Put_Line ("Reference time: 2:00 PM UTC (14:00)");
      Put_Line ("");

      for Participant_Zone_Name of Participants loop
         declare
            Zone_Result : constant ZoneInfo.Zone_Id_Result :=
              Domain.Value_Object.Zone_Id.Result.Create (Participant_Zone_Name.all);
         begin
            if Domain.Value_Object.Zone_Id.Result.Is_Ok (Zone_Result) then
               declare
                  Zone : constant ZoneInfo.Zone_Id :=
                    Domain.Value_Object.Zone_Id.Result.Value (Zone_Result);

                  Offset_Result : constant ZoneInfo.UTC_Offset_Result :=
                    ZoneInfo.API.Get_UTC_Offset (Zone, Reference_UTC);
               begin
                  if Domain.Value_Object.UTC_Offset.Result.Is_Ok (Offset_Result) then
                     declare
                        Offset : constant Duration :=
                          Duration (Domain.Value_Object.UTC_Offset.To_Seconds
                            (Domain.Value_Object.UTC_Offset.Result.Value (Offset_Result)));

                        TZ : constant Timezone_Info :=
                          (Name => To_Bounded_String (Participant_Zone_Name.all),
                           Zone_Id => Zone,
                           Offset => Offset);
                     begin
                        Print_Time_In_Timezone ("  ", Reference_UTC, TZ);
                     end;
                  end if;
               end;
            end if;
         end;
      end loop;
      Put_Line ("");
      Put_Line ("Analysis: 2 PM UTC works well for Europe and East Coast US,");
      Put_Line ("but is early morning (6 AM) for US West Coast and");
      Put_Line ("late evening (11 PM) for Tokyo.");
      Put_Line ("");
   end Example_3_Conference_Call;

begin
   Put_Line ("===========================================================");
   Put_Line ("  Practical Timezone Conversion Scenarios");
   Put_Line ("===========================================================");
   Put_Line ("");

   ZoneInfo.API.Initialize;

   Example_1_Global_Standup;
   Example_2_Market_Hours;
   Example_3_Conference_Call;

   Put_Line ("===========================================================");
   Put_Line ("  Best Practices:");
   Put_Line ("===========================================================");
   Put_Line ("1. Always work in UTC internally");
   Put_Line ("2. Convert to local time only for display");
   Put_Line ("3. Use ZoneInfo to get current UTC offset for any date");
   Put_Line ("4. Account for DST when scheduling recurring events");
   Put_Line ("5. Store timestamps in UTC, never in local time");
   Put_Line ("===========================================================");
end Timezone_Conversion;
