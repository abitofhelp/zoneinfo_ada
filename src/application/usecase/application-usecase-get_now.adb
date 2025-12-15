pragma Ada_2022;
--  =========================================================================
--  Application.Usecase.Get_Now - Implementation
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--  =========================================================================

with Domain.Error;
use Domain.Error;
use Domain.Error.Error_Strings;

package body Application.Usecase.Get_Now is

   --  ========================================================================
   --  Use Case Operations
   --  ========================================================================

   function Execute return Instant_Result.Result is
   begin
      --  Delegate to the injected clock adapter
      return Now;
   end Execute;

   function Execute_Zoned (Zone : Zone_ID) return Zoned_Result.Result is
      Clock_Result : constant Instant_Result.Result := Now;
   begin
      if Instant_Result.Is_Error (Clock_Result) then
         --  Propagate clock error
         declare
            Err : constant Error_Type :=
              Instant_Result.Error_Info (Clock_Result);
         begin
            return Zoned_Result.Error
              (Kind    => Err.Kind,
               Message => To_String (Err.Message));
         end;
      end if;

      --  Create Zoned from Instant + Zone
      return Zoned_Result.Ok
        (Domain.Value_Object.Zoned.Create
           (Instant_Value => Instant_Result.Value (Clock_Result),
            Zone          => Zone));
   end Execute_Zoned;

   function Execute_UTC return Zoned_Result.Result is
      package Zone_Pkg renames Domain.Value_Object.Zone_ID;
      UTC_Zone_Result : constant Zone_Pkg.Zone_ID_Result.Result :=
        Zone_Pkg.From_String ("UTC");
   begin
      --  UTC zone creation should never fail, but handle it gracefully
      if Zone_Pkg.Zone_ID_Result.Is_Error (UTC_Zone_Result) then
         return Zoned_Result.Error
           (Kind    => Internal_Error,
            Message => "Failed to create UTC zone identifier");
      end if;

      return Execute_Zoned (Zone_Pkg.Zone_ID_Result.Value (UTC_Zone_Result));
   end Execute_UTC;

end Application.Usecase.Get_Now;
