pragma Ada_2022;
--  =========================================================================
--  Workflow: Local Time to UTC to Another Zone
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Demonstrates the most common timezone workflow:
--    1. Get current local time
--    2. Convert to UTC
--    3. Calculate duration offset from local to UTC
--    4. Convert to another timezone (Pacific/Fiji)
--    5. Display all times as ISO 8601 strings
--
--  This is the "hello world" of timezone programming.
--  =========================================================================

with Ada.Text_IO;
with Zoneinfo.API.Desktop;
with Zoneinfo.API.Format;
with Zoneinfo.API.Discovery;

procedure Local_To_UTC_To_Zone is
   package TIO renames Ada.Text_IO;
   package Desktop renames Zoneinfo.API.Desktop;
   package Fmt renames Zoneinfo.API.Format;
   package Disc renames Zoneinfo.API.Discovery;
begin
   TIO.Put_Line ("==============================================");
   TIO.Put_Line ("Workflow: Local -> UTC -> Pacific/Fiji");
   TIO.Put_Line ("==============================================");
   TIO.New_Line;

   --  ===================================================================
   --  Step 1: Get local timezone
   --  ===================================================================
   TIO.Put_Line ("Step 1: Detecting local timezone...");

   declare
      My_Zone_Result : constant Disc.Zone_ID_Result.Result := Disc.Find_My_Id;
   begin
      if Disc.Is_Ok (My_Zone_Result) then
         TIO.Put_Line ("  Local zone: " &
           Disc.To_String (Disc.Value (My_Zone_Result)));
      else
         TIO.Put_Line ("  Could not detect local timezone");
      end if;
   end;
   TIO.New_Line;

   --  ===================================================================
   --  Step 2: Get current time as Instant
   --  ===================================================================
   TIO.Put_Line ("Step 2: Getting current time...");

   declare
      Now_Result : constant Desktop.Instant_Result.Result := Desktop.Now;
   begin
      if Desktop.Instant_Result.Is_Ok (Now_Result) then
         declare
            Current_Instant : constant Desktop.Instant :=
              Desktop.Instant_Result.Value (Now_Result);
         begin
            TIO.Put_Line ("  Instant (epoch): " &
              Fmt.To_String (Fmt.To_Epoch_String (Current_Instant)));

            --  ============================================================
            --  Step 3: Convert to UTC
            --  ============================================================
            TIO.New_Line;
            TIO.Put_Line ("Step 3: Converting to UTC...");

            declare
               UTC_Result : constant Desktop.Zoned_Result.Result :=
                 Desktop.Now_UTC;
            begin
               if Desktop.Zoned_Result.Is_Ok (UTC_Result) then
                  declare
                     UTC_Zoned : constant Desktop.Zoned :=
                       Desktop.Zoned_Result.Value (UTC_Result);
                     UTC_Civil : constant Desktop.Civil :=
                       Desktop.To_Civil (UTC_Zoned);
                     UTC_Offset : constant Desktop.Duration_Type :=
                       Desktop.Get_Offset (UTC_Zoned);
                  begin
                     TIO.Put_Line ("  UTC time: " &
                       Fmt.To_String (Fmt.To_ISO_8601_With_Offset
                         (UTC_Civil, UTC_Offset)));
                     TIO.Put_Line ("  UTC offset: " &
                       Fmt.To_String (Fmt.Format_Offset (UTC_Offset)));

                     --  =================================================
                     --  Step 4: Convert to Pacific/Fiji
                     --  =================================================
                     TIO.New_Line;
                     TIO.Put_Line ("Step 4: Converting to Pacific/Fiji...");

                     declare
                        Fiji_Zone_R : constant Desktop.Zone_ID_Result.Result :=
                          Desktop.Zone_From_String ("Pacific/Fiji");
                     begin
                        if Desktop.Zone_ID_Result.Is_Ok (Fiji_Zone_R) then
                           declare
                              Fiji_Zone : constant Desktop.Zone_ID :=
                                Desktop.Zone_ID_Result.Value (Fiji_Zone_R);
                              Fiji_Zoned : constant Desktop.Zoned :=
                                Desktop.With_Zone (UTC_Zoned, Fiji_Zone);
                              Fiji_Civil : constant Desktop.Civil :=
                                Desktop.To_Civil (Fiji_Zoned);
                              Fiji_Offset : constant Desktop.Duration_Type :=
                                Desktop.Get_Offset (Fiji_Zoned);
                           begin
                              TIO.Put_Line ("  Fiji time: " &
                                Fmt.To_String (Fmt.To_ISO_8601_Full
                                  (Fiji_Civil, Fiji_Offset, Fiji_Zone)));
                              TIO.Put_Line ("  Fiji offset: " &
                                Fmt.To_String
                                  (Fmt.Format_Offset (Fiji_Offset)));

                              --  ========================================
                              --  Step 5: Show duration offset
                              --  ========================================
                              TIO.New_Line;
                              TIO.Put_Line ("Step 5: Offset analysis...");
                              TIO.Put_Line ("  Fiji is " &
                                Fmt.To_String
                                  (Fmt.To_Human_Duration (Fiji_Offset))
                                & " ahead of UTC");
                           end;
                        else
                           TIO.Put_Line
                             ("  Error: Could not create Pacific/Fiji");
                        end if;
                     end;
                  end;
               else
                  TIO.Put_Line ("  Error: Could not get UTC time");
               end if;
            end;
         end;
      else
         TIO.Put_Line ("  Error: Could not get current time");
      end if;
   end;

   TIO.New_Line;
   TIO.Put_Line ("==============================================");
   TIO.Put_Line ("Workflow complete!");
   TIO.Put_Line ("==============================================");

end Local_To_UTC_To_Zone;
