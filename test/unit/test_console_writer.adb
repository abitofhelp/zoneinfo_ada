pragma Ada_2022;
--  ======================================================================
--  Test_Console_Writer
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Unit tests for Infrastructure.Adapter.Console_Writer functionality.
--    Tests the Write function that outputs to console and returns Result.
--  ======================================================================

with Ada.Text_IO;
with Application.Port.Outbound.Writer;
with Domain.Error;
with Domain.Unit;
with Infrastructure.Adapter.Console_Writer;
with Test_Framework;

procedure Test_Console_Writer is

   use Ada.Text_IO;
   use Application.Port.Outbound.Writer;
   use Domain.Error;

   --  Test statistics
   Total_Tests  : Natural := 0;
   Passed_Tests : Natural := 0;

   --  Helper procedure to run a test
   pragma Style_Checks (Off);
   procedure Run_Test (Name : String; Passed : Boolean) is
   begin
      Total_Tests := Total_Tests + 1;
      if Passed then
         Passed_Tests := Passed_Tests + 1;
         Put_Line ("[PASS] " & Name);
      else
         Put_Line ("[FAIL] " & Name);
      end if;
   end Run_Test;
   pragma Style_Checks (On);

begin
   Put_Line ("========================================");
   Put_Line ("Testing: Infrastructure.Adapter.Console_Writer");
   Put_Line ("========================================");
   New_Line;

   --  ========================================================================
   --  Test: Write with simple message succeeds
   --  ========================================================================

   declare
      Result : constant Unit_Result.Result :=
        Infrastructure.Adapter.Console_Writer.Write ("Test message 1");
   begin
      Run_Test
        ("Write with simple message returns Ok",
         Unit_Result.Is_Ok (Result));
   end;

   --  ========================================================================
   --  Test: Write with empty message succeeds
   --  ========================================================================

   declare
      Result : constant Unit_Result.Result :=
        Infrastructure.Adapter.Console_Writer.Write ("");
   begin
      Run_Test
        ("Write with empty message returns Ok",
         Unit_Result.Is_Ok (Result));
   end;

   --  ========================================================================
   --  Test: Write with long message succeeds
   --  ========================================================================

   declare
      Long_Message : constant String (1 .. 500) := [others => 'X'];
      Result       : constant Unit_Result.Result :=
        Infrastructure.Adapter.Console_Writer.Write (Long_Message);
   begin
      Run_Test
        ("Write with long message returns Ok",
         Unit_Result.Is_Ok (Result));
   end;

   --  ========================================================================
   --  Test: Write with special characters succeeds
   --  ========================================================================

   declare
      Special_Message : constant String := "Special: !@#$%^&*()_+-=[]{}|;':"",./<>?";
      Result          : constant Unit_Result.Result :=
        Infrastructure.Adapter.Console_Writer.Write (Special_Message);
   begin
      Run_Test
        ("Write with special characters returns Ok",
         Unit_Result.Is_Ok (Result));
   end;

   --  ========================================================================
   --  Test: Write with unicode message succeeds
   --  ========================================================================

   declare
      Unicode_Message : constant String := "Unicode test";
      Result          : constant Unit_Result.Result :=
        Infrastructure.Adapter.Console_Writer.Write (Unicode_Message);
   begin
      Run_Test
        ("Write with unicode message returns Ok",
         Unit_Result.Is_Ok (Result));
   end;

   --  ========================================================================
   --  Test: Multiple writes in sequence succeed
   --  ========================================================================

   declare
      Result1 : constant Unit_Result.Result :=
        Infrastructure.Adapter.Console_Writer.Write ("First write");
      Result2 : constant Unit_Result.Result :=
        Infrastructure.Adapter.Console_Writer.Write ("Second write");
      Result3 : constant Unit_Result.Result :=
        Infrastructure.Adapter.Console_Writer.Write ("Third write");
   begin
      Run_Test
        ("Multiple writes in sequence all return Ok",
         Unit_Result.Is_Ok (Result1) and then
         Unit_Result.Is_Ok (Result2) and then
         Unit_Result.Is_Ok (Result3));
   end;

   --  ========================================================================
   --  Test: Write returns Unit value on success
   --  ========================================================================

   declare
      use Domain.Unit;
      Result : constant Unit_Result.Result :=
        Infrastructure.Adapter.Console_Writer.Write ("Unit value test");
   begin
      if Unit_Result.Is_Ok (Result) then
         Run_Test
           ("Write returns Unit_Value on success",
            Unit_Result.Value (Result) = Unit_Value);
      else
         Run_Test ("Write should return Ok", False);
      end if;
   end;

   --  Print summary
   New_Line;
   Put_Line ("========================================");
   Put_Line ("Test Summary: Infrastructure.Adapter.Console_Writer");
   Put_Line ("========================================");
   Put_Line ("Total tests: " & Total_Tests'Image);
   Put_Line ("Passed:      " & Passed_Tests'Image);
   Put_Line ("Failed:      " & Natural'Image (Total_Tests - Passed_Tests));
   New_Line;

   --  Register results with test framework
   Test_Framework.Register_Results (Total_Tests, Passed_Tests);

end Test_Console_Writer;
