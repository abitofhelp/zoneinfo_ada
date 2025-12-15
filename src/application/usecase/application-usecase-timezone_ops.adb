pragma Ada_2022;
--  =========================================================================
--  Application.Usecase.Timezone_Ops - Implementation
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--  =========================================================================

with Domain.Error;
use Domain.Error;
use Domain.Error.Error_Strings;

package body Application.Usecase.Timezone_Ops is

   --  ========================================================================
   --  Instant/Zoned to Civil Conversion
   --  ========================================================================

   function Convert_To_Civil (Z : Zoned) return Civil is
   begin
      return To_Civil
        (I    => Domain.Value_Object.Zoned.To_Instant (Z),
         Zone => Domain.Value_Object.Zoned.Get_Zone (Z));
   end Convert_To_Civil;

   function Convert_To_Civil
     (I    : Instant;
      Zone : Zone_ID) return Civil
   is
   begin
      return To_Civil (I, Zone);
   end Convert_To_Civil;

   --  ========================================================================
   --  Civil to Instant/Zoned Conversion
   --  ========================================================================

   function Convert_To_Zoned
     (C    : Civil;
      Zone : Zone_ID) return Zoned_Result.Result
   is
      Instant_Res : constant Instant_Result.Result := To_Instant (C, Zone);
   begin
      if Instant_Result.Is_Error (Instant_Res) then
         declare
            Err : constant Error_Type :=
              Instant_Result.Error_Info (Instant_Res);
         begin
            return Zoned_Result.Error
              (Kind    => Err.Kind,
               Message => Error_Strings.To_String (Err.Message));
         end;
      end if;

      return Zoned_Result.Ok
        (Domain.Value_Object.Zoned.Create
           (Instant_Value => Instant_Result.Value (Instant_Res),
            Zone          => Zone));
   end Convert_To_Zoned;

   function Convert_To_Instant
     (C    : Civil;
      Zone : Zone_ID) return Instant_Result.Result
   is
   begin
      return To_Instant (C, Zone);
   end Convert_To_Instant;

   --  ========================================================================
   --  Zone Validation
   --  ========================================================================

   function Validate_Zone (Zone : Zone_ID) return Boolean is
   begin
      return Is_Valid_Zone (Zone);
   end Validate_Zone;

   --  ========================================================================
   --  UTC Offset Query
   --  ========================================================================

   function Get_Offset (Z : Zoned) return Duration_Type is
   begin
      return Get_UTC_Offset
        (I    => Domain.Value_Object.Zoned.To_Instant (Z),
         Zone => Domain.Value_Object.Zoned.Get_Zone (Z));
   end Get_Offset;

   function Get_Offset
     (I    : Instant;
      Zone : Zone_ID) return Duration_Type
   is
   begin
      return Get_UTC_Offset (I, Zone);
   end Get_Offset;

end Application.Usecase.Timezone_Ops;
