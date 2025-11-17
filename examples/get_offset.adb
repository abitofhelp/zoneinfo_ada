pragma Ada_2022;
--  ===========================================================================
--  Get UTC Offset for a Timezone
--  ===========================================================================
--  Demonstrates:
--    - Creating a Zone_Id
--    - Getting current UTC offset
--    - Railway-oriented error handling
--  ===========================================================================

with Ada.Text_IO;
with Ada.Calendar;
with ZoneInfo;
with ZoneInfo.API;
with Domain.Value_Object.Zone_Id;
with Domain.Value_Object.Zone_Id.Result;
with Domain.Value_Object.UTC_Offset;
with Domain.Value_Object.UTC_Offset.Result;

procedure Get_Offset is

   use Ada.Text_IO;

   --  Create a Zone_Id with validation
   Zone_Result : constant ZoneInfo.Zone_Id_Result :=
     Domain.Value_Object.Zone_Id.Result.Create ("America/New_York");

begin
   Put_Line ("========================================================");
   Put_Line ("  ZoneInfo Example: Get UTC Offset");
   Put_Line ("========================================================");
   Put_Line ("");

   --  Initialize timezone database
   ZoneInfo.API.Initialize;

   --  Check if zone creation succeeded
   if Domain.Value_Object.Zone_Id.Result.Is_Ok (Zone_Result) then
      declare
         NYC_Zone : constant ZoneInfo.Zone_Id :=
           Domain.Value_Object.Zone_Id.Result.Value (Zone_Result);
         Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;

         --  Get UTC offset for current time in New York
         Offset_Result : constant ZoneInfo.UTC_Offset_Result :=
           ZoneInfo.API.Get_UTC_Offset (NYC_Zone, Now);
      begin
         Put_Line ("Zone: " &
                  Domain.Value_Object.Zone_Id.To_String (NYC_Zone));
         Put_Line ("");

         if Domain.Value_Object.UTC_Offset.Result.Is_Ok (Offset_Result) then
            declare
               Offset : constant ZoneInfo.UTC_Offset :=
                 Domain.Value_Object.UTC_Offset.Result.Value (Offset_Result);
               Offset_Str : constant String :=
                 Domain.Value_Object.UTC_Offset.To_String (Offset);
            begin
               Put_Line ("SUCCESS: Current UTC Offset: " & Offset_Str);
               Put_Line ("");
               Put_Line ("Timezone offset retrieved successfully.");
            end;
         else
            Put_Line ("ERROR: Failed getting offset");
         end if;
      end;
   else
      Put_Line ("ERROR: Failed creating zone");
   end if;

   Put_Line ("");
   Put_Line ("========================================================");

end Get_Offset;
