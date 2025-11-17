pragma Ada_2022;
--  ===========================================================================
--  Domain.Value_Object.Zone_Id.Result
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Result value object - immutable domain data.
--
--  ===========================================================================

with Ada.Characters.Handling;

package body Domain.Value_Object.Zone_Id.Result is

   use Domain.Error;

   --  ========================================================================
   --  Validation Helpers (internal)
   --  ========================================================================

   --  Check if character is valid for zone ID
   --  Valid: a-z, A-Z, 0-9, _, -, /, +
   function Is_Valid_Char (C : Character) return Boolean is
      use Ada.Characters.Handling;
   begin
      return Is_Alphanumeric (C) or else C in '_' | '-' | '/' | '+';
   end Is_Valid_Char;

   --  Validate zone ID string
   function Is_Valid_Zone_Id (ID : String) return Boolean is
   begin
      --  Check length
      if ID'Length = 0 or else ID'Length > Max_Zone_Id_Length then
         return False;
      end if;

      --  Check each character
      for C of ID loop
         if not Is_Valid_Char (C) then
            return False;
         end if;
      end loop;

      return True;
   end Is_Valid_Zone_Id;

   --  ========================================================================
   --  Smart Constructor
   --  ========================================================================

   function Create (ID : String) return Result is
      package Strings renames Domain.Value_Object.Zone_Id.Zone_Id_Strings;
   begin
      --  Validate
      if not Is_Valid_Zone_Id (ID) then
         if ID'Length = 0 then
            return Error
              (Kind    => Validation_Error,
               Message => "Zone ID cannot be empty");
         elsif ID'Length > Max_Zone_Id_Length then
            return Error
              (Kind    => Validation_Error,
               Message => "Zone ID exceeds maximum length of" &
                         Max_Zone_Id_Length'Image & " characters");
         else
            return Error
              (Kind    => Validation_Error,
               Message => "Zone ID contains invalid characters: " & ID);
         end if;
      end if;

      --  Create Zone_Id
      return Ok (Domain.Value_Object.Zone_Id.Zone_Id_Type'
        (ID => Strings.To_Bounded_String (ID)));
   end Create;

end Domain.Value_Object.Zone_Id.Result;
