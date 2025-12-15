pragma Ada_2022;
--  ======================================================================
--  Test_Domain_Zone_ID
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Unit tests for Domain.Value_Object.Zone_ID functionality.
--    Tests creation, validation, and comparisons.
--  ======================================================================

with Ada.Text_IO;
with Domain.Value_Object.Zone_ID;
with Test_Framework;

procedure Test_Domain_Zone_ID is

   use Ada.Text_IO;
   use Domain.Value_Object.Zone_ID;

   --  Test statistics
   Total_Tests  : Natural := 0;
   Passed_Tests : Natural := 0;

   procedure Run_Test (Name : String; Passed : Boolean) is
   begin
      Total_Tests := Total_Tests + 1;
      if Passed then
         Passed_Tests := Passed_Tests + 1;
         Put_Line ("  [PASS] " & Name);
      else
         Put_Line ("  [FAIL] " & Name);
      end if;
   end Run_Test;

begin
   Put_Line ("========================================");
   Put_Line ("Testing: Domain.Value_Object.Zone_ID");
   Put_Line ("========================================");
   New_Line;

   --  ========================================================================
   --  Test: Valid Zone_ID creation
   --  ========================================================================

   Put_Line ("Test: Valid Zone_ID Creation");

   declare
      R : constant Zone_ID_Result.Result := From_String ("UTC");
   begin
      Run_Test ("From_String(UTC) - Is_Ok", Zone_ID_Result.Is_Ok (R));
      if Zone_ID_Result.Is_Ok (R) then
         Run_Test
           ("UTC To_String",
            To_String (Zone_ID_Result.Value (R)) = "UTC");
      end if;
   end;

   declare
      R : constant Zone_ID_Result.Result := From_String ("America/New_York");
   begin
      Run_Test
        ("From_String(America/New_York) - Is_Ok",
         Zone_ID_Result.Is_Ok (R));
      if Zone_ID_Result.Is_Ok (R) then
         Run_Test
           ("America/New_York To_String",
            To_String (Zone_ID_Result.Value (R)) = "America/New_York");
      end if;
   end;

   declare
      R : constant Zone_ID_Result.Result := From_String ("Europe/London");
   begin
      Run_Test
        ("From_String(Europe/London) - Is_Ok", Zone_ID_Result.Is_Ok (R));
   end;

   --  ========================================================================
   --  Test: Invalid Zone_ID (empty string)
   --  ========================================================================

   Put_Line ("Test: Invalid Zone_ID - Empty String");

   declare
      R : constant Zone_ID_Result.Result := From_String ("");
   begin
      Run_Test ("Empty string - Is_Error", Zone_ID_Result.Is_Error (R));
   end;

   --  ========================================================================
   --  Test: Is_UTC helper
   --  ========================================================================

   Put_Line ("Test: Is_UTC Helper");

   declare
      R : constant Zone_ID_Result.Result := From_String ("UTC");
   begin
      if Zone_ID_Result.Is_Ok (R) then
         Run_Test ("Is_UTC for UTC zone", Is_UTC (Zone_ID_Result.Value (R)));
      else
         Run_Test ("Is_UTC test setup", False);
      end if;
   end;

   declare
      R : constant Zone_ID_Result.Result := From_String ("America/New_York");
   begin
      if Zone_ID_Result.Is_Ok (R) then
         Run_Test
           ("Not Is_UTC for America/New_York",
            not Is_UTC (Zone_ID_Result.Value (R)));
      else
         Run_Test ("Not Is_UTC test setup", False);
      end if;
   end;

   --  ========================================================================
   --  Test: Zone_ID equality
   --  ========================================================================

   Put_Line ("Test: Zone_ID Equality");

   declare
      R1 : constant Zone_ID_Result.Result := From_String ("UTC");
      R2 : constant Zone_ID_Result.Result := From_String ("UTC");
      R3 : constant Zone_ID_Result.Result := From_String ("America/New_York");
   begin
      if Zone_ID_Result.Is_Ok (R1) and then Zone_ID_Result.Is_Ok (R2) and then
         Zone_ID_Result.Is_Ok (R3)
      then
         Run_Test
           ("Equal Zone_IDs",
            Zone_ID_Result.Value (R1) = Zone_ID_Result.Value (R2));
         Run_Test
           ("Different Zone_IDs",
            not (Zone_ID_Result.Value (R1) = Zone_ID_Result.Value (R3)));
      else
         Run_Test ("Zone_ID equality test setup", False);
      end if;
   end;

   --  ========================================================================
   --  Test: Zone_ID with various IANA zone formats
   --  ========================================================================

   Put_Line ("Test: Various IANA Zone Formats");

   declare
      R : constant Zone_ID_Result.Result := From_String ("Asia/Tokyo");
   begin
      Run_Test ("Asia/Tokyo - Is_Ok", Zone_ID_Result.Is_Ok (R));
   end;

   declare
      R : constant Zone_ID_Result.Result :=
        From_String ("America/Argentina/Buenos_Aires");
   begin
      Run_Test
        ("America/Argentina/Buenos_Aires - Is_Ok",
         Zone_ID_Result.Is_Ok (R));
   end;

   declare
      R : constant Zone_ID_Result.Result := From_String ("Etc/GMT+5");
   begin
      Run_Test ("Etc/GMT+5 - Is_Ok", Zone_ID_Result.Is_Ok (R));
   end;

   --  Print summary
   New_Line;
   Put_Line ("========================================");
   Put_Line ("Test Summary: Domain.Value_Object.Zone_ID");
   Put_Line ("========================================");
   Put_Line ("Total tests: " & Total_Tests'Image);
   Put_Line ("Passed:      " & Passed_Tests'Image);
   Put_Line ("Failed:      " & Natural'Image (Total_Tests - Passed_Tests));
   New_Line;

   --  Register results with test framework
   Test_Framework.Register_Results (Total_Tests, Passed_Tests);

end Test_Domain_Zone_ID;
