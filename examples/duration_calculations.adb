pragma Ada_2022;
-- ===========================================================================
--  Duration Calculations Example - Working with Ada.Calendar.Duration
-- ===========================================================================
--  Demonstrates how to calculate and work with Duration values across
--  timezones, including:
--    - Computing time spans between events
--    - Converting Duration to hours/minutes/seconds
--    - Handling timezone-aware duration calculations
--    - Working with meeting times across global teams
-- ===========================================================================

with Ada.Text_IO;
with Ada.Calendar;
with Ada.Calendar.Formatting;
with ZoneInfo;
with ZoneInfo.API;
with Domain.Value_Object.Zone_Id;
with Domain.Value_Object.Zone_Id.Result;
with Domain.Value_Object.UTC_Offset;
with Domain.Value_Object.UTC_Offset.Result;

procedure Duration_Calculations is

   use Ada.Text_IO;
   use type Ada.Calendar.Time;

   procedure Print_Duration (Label : String; D : Duration);

   procedure Print_Duration (Label : String; D : Duration) is
      Total_Seconds : constant Natural := Natural (D);
      Hours : constant Natural := Total_Seconds / 3600;
      Minutes : constant Natural := (Total_Seconds rem 3600) / 60;
      Seconds : constant Natural := Total_Seconds rem 60;
   begin
      Put_Line (Label & ":" & Hours'Image & "h" &
               Minutes'Image & "m" &
               Seconds'Image & "s" &
               " (" & Total_Seconds'Image & " seconds)");
   end Print_Duration;

   procedure Example_1_Simple_Duration;
   procedure Example_2_Work_Hours;
   procedure Example_3_Global_Meeting_Duration;
   procedure Example_4_Flight_Time_Calculation;

   procedure Example_1_Simple_Duration is
      Start_Time : constant Ada.Calendar.Time :=
        Ada.Calendar.Time_Of (2024, 7, 15, 9.0 * 3600.0);    -- 9 AM

      End_Time : constant Ada.Calendar.Time :=
        Ada.Calendar.Time_Of (2024, 7, 15, 17.5 * 3600.0);   -- 5:30 PM

      Work_Duration : constant Duration := End_Time - Start_Time;
   begin
      Put_Line ("=== Example 1: Simple Duration Calculation ===");
      Put_Line ("");
      Put_Line ("Calculate work hours in a day:");
      Put_Line ("");

      Print_Duration ("Work day duration", Work_Duration);
      Put_Line ("");

      declare
         Hours : constant Float := Float (Work_Duration) / 3600.0;
      begin
         Put_Line ("In hours: " & Hours'Image);
      end;
      Put_Line ("");
   end Example_1_Simple_Duration;

   procedure Example_2_Work_Hours is
      Monday_Start : constant Ada.Calendar.Time :=
        Ada.Calendar.Time_Of (2024, 7, 15, 8.5 * 3600.0);   -- 8:30 AM

      Monday_End : constant Ada.Calendar.Time :=
        Ada.Calendar.Time_Of (2024, 7, 15, 17.25 * 3600.0); -- 5:15 PM

      Lunch_Break : constant Duration := 45.0 * 60.0;  -- 45 minutes

      Total_Time : constant Duration := Monday_End - Monday_Start;
      Actual_Work : constant Duration := Total_Time - Lunch_Break;
   begin
      Put_Line ("=== Example 2: Work Hours with Break ===");
      Put_Line ("");

      Print_Duration ("Total time at office", Total_Time);
      Print_Duration ("Lunch break", Lunch_Break);
      Print_Duration ("Actual work time", Actual_Work);
      Put_Line ("");
   end Example_2_Work_Hours;

   procedure Example_3_Global_Meeting_Duration is
      London_Zone_Result : constant ZoneInfo.Zone_Id_Result :=
        Domain.Value_Object.Zone_Id.Result.Create ("Europe/London");

      Tokyo_Zone_Result : constant ZoneInfo.Zone_Id_Result :=
        Domain.Value_Object.Zone_Id.Result.Create ("Asia/Tokyo");

      --  Meeting starts 10 AM London time
      London_Start_Local : constant Ada.Calendar.Time :=
        Ada.Calendar.Time_Of (2024, 11, 20, 10.0 * 3600.0);

      --  Meeting ends 5 PM Tokyo time (same day)
      Tokyo_End_Local : constant Ada.Calendar.Time :=
        Ada.Calendar.Time_Of (2024, 11, 20, 17.0 * 3600.0);
   begin
      Put_Line ("=== Example 3: Global Meeting Duration ===");
      Put_Line ("");
      Put_Line ("Question: A meeting starts at 10 AM London and ends at 5 PM Tokyo.");
      Put_Line ("How long is the meeting?");
      Put_Line ("");

      if Domain.Value_Object.Zone_Id.Result.Is_Ok (London_Zone_Result) and then
         Domain.Value_Object.Zone_Id.Result.Is_Ok (Tokyo_Zone_Result)
      then
         declare
            London_Zone : constant ZoneInfo.Zone_Id :=
              Domain.Value_Object.Zone_Id.Result.Value (London_Zone_Result);

            Tokyo_Zone : constant ZoneInfo.Zone_Id :=
              Domain.Value_Object.Zone_Id.Result.Value (Tokyo_Zone_Result);

            London_Offset_Result : constant ZoneInfo.UTC_Offset_Result :=
              ZoneInfo.API.Get_UTC_Offset (London_Zone, London_Start_Local);

            Tokyo_Offset_Result : constant ZoneInfo.UTC_Offset_Result :=
              ZoneInfo.API.Get_UTC_Offset (Tokyo_Zone, Tokyo_End_Local);
         begin
            if Domain.Value_Object.UTC_Offset.Result.Is_Ok (London_Offset_Result) and then
               Domain.Value_Object.UTC_Offset.Result.Is_Ok (Tokyo_Offset_Result)
            then
               declare
                  London_Offset : constant ZoneInfo.UTC_Offset :=
                    Domain.Value_Object.UTC_Offset.Result.Value (London_Offset_Result);

                  Tokyo_Offset : constant ZoneInfo.UTC_Offset :=
                    Domain.Value_Object.UTC_Offset.Result.Value (Tokyo_Offset_Result);

                  London_Offset_Duration : constant Duration :=
                    Duration (Domain.Value_Object.UTC_Offset.To_Seconds (London_Offset));

                  Tokyo_Offset_Duration : constant Duration :=
                    Duration (Domain.Value_Object.UTC_Offset.To_Seconds (Tokyo_Offset));

                  --  Convert both to UTC
                  London_Start_UTC : constant Ada.Calendar.Time :=
                    London_Start_Local - London_Offset_Duration;

                  Tokyo_End_UTC : constant Ada.Calendar.Time :=
                    Tokyo_End_Local - Tokyo_Offset_Duration;

                  --  Calculate duration in UTC time
                  Meeting_Duration : constant Duration := Tokyo_End_UTC - London_Start_UTC;
               begin
                  Put_Line ("Start: 10:00 AM London (GMT" &
                           London_Offset_Duration'Image & "s)");
                  Put_Line ("End:    5:00 PM Tokyo (GMT" &
                           Tokyo_Offset_Duration'Image & "s)");
                  Put_Line ("");

                  Print_Duration ("Meeting duration", Meeting_Duration);
                  Put_Line ("");

                  Put_Line ("Note: Always convert to UTC before calculating durations");
                  Put_Line ("across timezones to avoid DST and offset errors!");
               end;
            end if;
         end;
      end if;
      Put_Line ("");
   end Example_3_Global_Meeting_Duration;

   procedure Example_4_Flight_Time_Calculation is
      --  Flight departs 2 PM Los Angeles, arrives 10 AM next day London
      LA_Depart : constant Ada.Calendar.Time :=
        Ada.Calendar.Time_Of (2024, 7, 15, 14.0 * 3600.0);  -- 2 PM

      London_Arrive : constant Ada.Calendar.Time :=
        Ada.Calendar.Time_Of (2024, 7, 16, 10.0 * 3600.0);  -- 10 AM next day

      LA_Zone_Result : constant ZoneInfo.Zone_Id_Result :=
        Domain.Value_Object.Zone_Id.Result.Create ("America/Los_Angeles");

      London_Zone_Result : constant ZoneInfo.Zone_Id_Result :=
        Domain.Value_Object.Zone_Id.Result.Create ("Europe/London");
   begin
      Put_Line ("=== Example 4: Flight Time Calculation ===");
      Put_Line ("");
      Put_Line ("Flight: Los Angeles -> London");
      Put_Line ("Departs: 2:00 PM LA time");
      Put_Line ("Arrives: 10:00 AM London time (next day)");
      Put_Line ("");

      if Domain.Value_Object.Zone_Id.Result.Is_Ok (LA_Zone_Result) and then
         Domain.Value_Object.Zone_Id.Result.Is_Ok (London_Zone_Result)
      then
         declare
            LA_Zone : constant ZoneInfo.Zone_Id :=
              Domain.Value_Object.Zone_Id.Result.Value (LA_Zone_Result);

            London_Zone : constant ZoneInfo.Zone_Id :=
              Domain.Value_Object.Zone_Id.Result.Value (London_Zone_Result);

            LA_Offset_Result : constant ZoneInfo.UTC_Offset_Result :=
              ZoneInfo.API.Get_UTC_Offset (LA_Zone, LA_Depart);

            London_Offset_Result : constant ZoneInfo.UTC_Offset_Result :=
              ZoneInfo.API.Get_UTC_Offset (London_Zone, London_Arrive);
         begin
            if Domain.Value_Object.UTC_Offset.Result.Is_Ok (LA_Offset_Result) and then
               Domain.Value_Object.UTC_Offset.Result.Is_Ok (London_Offset_Result)
            then
               declare
                  LA_Offset : constant ZoneInfo.UTC_Offset :=
                    Domain.Value_Object.UTC_Offset.Result.Value (LA_Offset_Result);

                  London_Offset : constant ZoneInfo.UTC_Offset :=
                    Domain.Value_Object.UTC_Offset.Result.Value (London_Offset_Result);

                  LA_Offset_Duration : constant Duration :=
                    Duration (Domain.Value_Object.UTC_Offset.To_Seconds (LA_Offset));

                  London_Offset_Duration : constant Duration :=
                    Duration (Domain.Value_Object.UTC_Offset.To_Seconds (London_Offset));

                  --  Convert to UTC
                  Depart_UTC : constant Ada.Calendar.Time := LA_Depart - LA_Offset_Duration;
                  Arrive_UTC : constant Ada.Calendar.Time := London_Arrive - London_Offset_Duration;

                  Flight_Duration : constant Duration := Arrive_UTC - Depart_UTC;
               begin
                  Print_Duration ("Actual flight time", Flight_Duration);
                  Put_Line ("");

                  Put_Line ("This is the actual time in the air, not the");
                  Put_Line ("'clock time difference' between LA and London!");
               end;
            end if;
         end;
      end if;
      Put_Line ("");
   end Example_4_Flight_Time_Calculation;

begin
   Put_Line ("===========================================================");
   Put_Line ("  Duration Calculations with ZoneInfo");
   Put_Line ("===========================================================");
   Put_Line ("");

   ZoneInfo.API.Initialize;

   Example_1_Simple_Duration;
   Example_2_Work_Hours;
   Example_3_Global_Meeting_Duration;
   Example_4_Flight_Time_Calculation;

   Put_Line ("===========================================================");
   Put_Line ("  Key Principles:");
   Put_Line ("===========================================================");
   Put_Line ("1. Duration = Time - Time (always in seconds)");
   Put_Line ("2. ALWAYS convert to UTC before cross-timezone durations");
   Put_Line ("3. Duration can be added/subtracted from Time");
   Put_Line ("4. Duration is type-safe (prevents common errors)");
   Put_Line ("5. ZoneInfo ensures correct UTC offset for any date/time");
   Put_Line ("===========================================================");
end Duration_Calculations;
