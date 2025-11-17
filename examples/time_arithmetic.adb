pragma Ada_2022;
-- ===========================================================================
--  Time Arithmetic Example - Using Ada.Calendar with ZoneInfo
-- ===========================================================================
--  Demonstrates how to perform time arithmetic across timezones using
--  Ada.Calendar.Time and Duration types with ZoneInfo UTC offset lookups.
--
--  Shows:
--    - Adding/subtracting durations to/from Ada.Calendar.Time
--    - Converting local time to UTC and back
--    - Calculating future/past times in different timezones
--    - Working with Ada.Calendar."-" and "+" operators
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

procedure Time_Arithmetic is

   use Ada.Text_IO;
   use type Ada.Calendar.Time;

   procedure Print_Time (Label : String; T : Ada.Calendar.Time);

   procedure Print_Time (Label : String; T : Ada.Calendar.Time) is
      use Ada.Calendar;
      use Ada.Calendar.Formatting;
      Year   : Year_Number;
      Month  : Month_Number;
      Day    : Day_Number;
      Seconds : Day_Duration;
   begin
      Split (T, Year, Month, Day, Seconds);
      Put_Line (Label & ": " &
               Year'Image & "-" &
               (if Month < 10 then "0" else "") & Month'Image & "-" &
               (if Day < 10 then "0" else "") & Day'Image & " " &
               Image (Seconds));
   end Print_Time;

   procedure Example_1_Add_Hours;
   procedure Example_2_Timezone_Conversion;
   procedure Example_3_Future_Meeting_Time;
   procedure Example_4_Time_Differences;

   procedure Example_1_Add_Hours is
      NY_Zone_Result : constant ZoneInfo.Zone_Id_Result :=
        Domain.Value_Object.Zone_Id.Result.Create ("America/New_York");

      Current_Time : constant Ada.Calendar.Time :=
        Ada.Calendar.Time_Of (2024, 7, 15, 14.0 * 3600.0);  -- 2 PM July 15

      Three_Hours : constant Duration := 3.0 * 3600.0;  -- 3 hours in seconds
      Future_Time : constant Ada.Calendar.Time := Current_Time + Three_Hours;
   begin
      Put_Line ("=== Example 1: Adding Hours to a Time ===");
      Put_Line ("");

      Print_Time ("Current time (2 PM)", Current_Time);
      Print_Time ("After adding 3 hours (5 PM)", Future_Time);
      Put_Line ("");

      Put_Line ("Note: Ada.Calendar.Time supports direct arithmetic:");
      Put_Line ("  Time + Duration = Time");
      Put_Line ("  Time - Duration = Time");
      Put_Line ("  Time - Time = Duration");
      Put_Line ("");
   end Example_1_Add_Hours;

   procedure Example_2_Timezone_Conversion is
      NY_Zone_Result : constant ZoneInfo.Zone_Id_Result :=
        Domain.Value_Object.Zone_Id.Result.Create ("America/New_York");

      Tokyo_Zone_Result : constant ZoneInfo.Zone_Id_Result :=
        Domain.Value_Object.Zone_Id.Result.Create ("Asia/Tokyo");

      NY_Local_Time : constant Ada.Calendar.Time :=
        Ada.Calendar.Time_Of (2024, 7, 15, 15.0 * 3600.0);  -- 3 PM in NY
   begin
      Put_Line ("=== Example 2: Converting Between Timezones ===");
      Put_Line ("");
      Put_Line ("Question: What time is it in Tokyo when it's 3 PM in New York?");
      Put_Line ("");

      if Domain.Value_Object.Zone_Id.Result.Is_Ok (NY_Zone_Result) and then
         Domain.Value_Object.Zone_Id.Result.Is_Ok (Tokyo_Zone_Result)
      then
         declare
            NY_Zone : constant ZoneInfo.Zone_Id :=
              Domain.Value_Object.Zone_Id.Result.Value (NY_Zone_Result);

            Tokyo_Zone : constant ZoneInfo.Zone_Id :=
              Domain.Value_Object.Zone_Id.Result.Value (Tokyo_Zone_Result);

            NY_Offset_Result : constant ZoneInfo.UTC_Offset_Result :=
              ZoneInfo.API.Get_UTC_Offset (NY_Zone, NY_Local_Time);

            Tokyo_Offset_Result : constant ZoneInfo.UTC_Offset_Result :=
              ZoneInfo.API.Get_UTC_Offset (Tokyo_Zone, NY_Local_Time);
         begin
            if Domain.Value_Object.UTC_Offset.Result.Is_Ok (NY_Offset_Result) and then
               Domain.Value_Object.UTC_Offset.Result.Is_Ok (Tokyo_Offset_Result)
            then
               declare
                  NY_Offset : constant ZoneInfo.UTC_Offset :=
                    Domain.Value_Object.UTC_Offset.Result.Value (NY_Offset_Result);

                  Tokyo_Offset : constant ZoneInfo.UTC_Offset :=
                    Domain.Value_Object.UTC_Offset.Result.Value (Tokyo_Offset_Result);

                  NY_Offset_Duration : constant Duration :=
                    Duration (Domain.Value_Object.UTC_Offset.To_Seconds (NY_Offset));

                  Tokyo_Offset_Duration : constant Duration :=
                    Duration (Domain.Value_Object.UTC_Offset.To_Seconds (Tokyo_Offset));

                  --  Convert: NY local -> UTC -> Tokyo local
                  UTC_Time : constant Ada.Calendar.Time :=
                    NY_Local_Time - NY_Offset_Duration;

                  Tokyo_Local_Time : constant Ada.Calendar.Time :=
                    UTC_Time + Tokyo_Offset_Duration;
               begin
                  Print_Time ("New York local time", NY_Local_Time);
                  Put_Line ("NY UTC offset: " & NY_Offset_Duration'Image & " seconds");
                  Put_Line ("");

                  Print_Time ("Converted to UTC", UTC_Time);
                  Put_Line ("");

                  Print_Time ("Tokyo local time", Tokyo_Local_Time);
                  Put_Line ("Tokyo UTC offset: " & Tokyo_Offset_Duration'Image & " seconds");
                  Put_Line ("");

                  Put_Line ("Result: 3 PM in New York = 4 AM next day in Tokyo");
                  Put_Line ("(13 hour time difference in summer)");
               end;
            end if;
         end;
      end if;
      Put_Line ("");
   end Example_2_Timezone_Conversion;

   procedure Example_3_Future_Meeting_Time is
      London_Zone_Result : constant ZoneInfo.Zone_Id_Result :=
        Domain.Value_Object.Zone_Id.Result.Create ("Europe/London");

      Meeting_Start : constant Ada.Calendar.Time :=
        Ada.Calendar.Time_Of (2024, 11, 20, 10.0 * 3600.0);  -- 10 AM

      Meeting_Duration : constant Duration := 2.5 * 3600.0;  -- 2.5 hours
      Meeting_End : constant Ada.Calendar.Time := Meeting_Start + Meeting_Duration;
   begin
      Put_Line ("=== Example 3: Calculate Future Meeting Time ===");
      Put_Line ("");
      Put_Line ("A meeting starts at 10 AM London time and lasts 2.5 hours");
      Put_Line ("");

      Print_Time ("Meeting starts", Meeting_Start);
      Put_Line ("Meeting duration: 2.5 hours");
      Print_Time ("Meeting ends", Meeting_End);
      Put_Line ("");
   end Example_3_Future_Meeting_Time;

   procedure Example_4_Time_Differences is
      Event1 : constant Ada.Calendar.Time :=
        Ada.Calendar.Time_Of (2024, 7, 15, 9.0 * 3600.0);   -- 9 AM

      Event2 : constant Ada.Calendar.Time :=
        Ada.Calendar.Time_Of (2024, 7, 15, 17.0 * 3600.0);  -- 5 PM

      Time_Between : constant Duration := Event2 - Event1;
      Hours_Between : constant Natural := Natural (Time_Between / 3600.0);
   begin
      Put_Line ("=== Example 4: Calculate Time Difference ===");
      Put_Line ("");
      Put_Line ("How much time between morning and evening events?");
      Put_Line ("");

      Print_Time ("Morning event", Event1);
      Print_Time ("Evening event", Event2);
      Put_Line ("");
      Put_Line ("Time difference:" & Hours_Between'Image & " hours");
      Put_Line ("Time difference: " & Time_Between'Image & " seconds");
      Put_Line ("");
   end Example_4_Time_Differences;

begin
   Put_Line ("===========================================================");
   Put_Line ("  Time Arithmetic with Ada.Calendar and ZoneInfo");
   Put_Line ("===========================================================");
   Put_Line ("");

   ZoneInfo.API.Initialize;

   Example_1_Add_Hours;
   Example_2_Timezone_Conversion;
   Example_3_Future_Meeting_Time;
   Example_4_Time_Differences;

   Put_Line ("===========================================================");
   Put_Line ("  Key Takeaways:");
   Put_Line ("===========================================================");
   Put_Line ("1. Ada.Calendar.Time supports + and - with Duration");
   Put_Line ("2. Time - Time = Duration (time difference)");
   Put_Line ("3. ZoneInfo provides UTC offsets as Duration");
   Put_Line ("4. Convert: Local -> UTC -> Different Timezone");
   Put_Line ("5. All arithmetic is type-safe and clear");
   Put_Line ("===========================================================");
end Time_Arithmetic;
