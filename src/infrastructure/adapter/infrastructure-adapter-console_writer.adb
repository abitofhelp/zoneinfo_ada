pragma Ada_2022;
--  =========================================================================
--  Infrastructure.Adapter.Console_Writer - Console output implementation
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Implements Write function that outputs text to console via Ada.Text_IO,
--    wrapping exceptions into Result type for railway-oriented error handling.
--
--  Implementation Notes:
--    Uses Functional.Try.Map_To_Result_With_Param for declarative exception
--    mapping. Returns Domain.Result directly (no Functional.Result
--    intermediary) per DDD/Clean/Hex architecture: core layers use
--    Domain types only.
--  =========================================================================

with Ada.Text_IO;
with Domain.Error;
with Domain.Unit;
with Functional.Try.Map_To_Result_With_Param;

package body Infrastructure.Adapter.Console_Writer is

   use Application.Port.Outbound.Writer;
   use Domain.Unit;

   --  ========================================================================
   --  Internal: Raw Write Action (returns Domain.Result for Map_To_Result)
   --  ========================================================================

   --  This function performs the actual I/O and may raise exceptions.
   --  Returns Domain.Result type directly for Map_To_Result_With_Param.
   function Raw_Write_Action (Message : String) return Unit_Result.Result is
   begin
      Ada.Text_IO.Put_Line (Message);
      return Unit_Result.Ok (Unit_Value);
   end Raw_Write_Action;

   --  ========================================================================
   --  Internal: Make_Error for console write operation
   --  ========================================================================

   function Make_Write_Error
     (Kind : Domain.Error.Error_Kind; Message : String)
      return Unit_Result.Result
   is
      pragma Unreferenced (Kind);
   begin
      return Unit_Result.Error
        (Domain.Error.IO_Error,
         "Console write failed: " & Message);
   end Make_Write_Error;

   --  ========================================================================
   --  Instantiate Map_To_Result_With_Param for Write Operation
   --  ========================================================================

   package Try_Write is new Functional.Try.Map_To_Result_With_Param
     (Error_Kind_Type    => Domain.Error.Error_Kind,
      Param_Type         => String,
      Result_Type        => Unit_Result.Result,
      Make_Error         => Make_Write_Error,
      Default_Error_Kind => Domain.Error.IO_Error,
      Action             => Raw_Write_Action);

   --  All exceptions map to IO_Error (default), so empty mappings
   Write_Mappings : constant Try_Write.Mapping_Array :=
     Try_Write.Empty_Mappings;

   -----------
   -- Write --
   -----------

   function Write
     (Message : String)
      return Application.Port.Outbound.Writer.Unit_Result.Result
   is
   begin
      return Try_Write.Run (Message, Write_Mappings);
   end Write;

end Infrastructure.Adapter.Console_Writer;
