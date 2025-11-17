pragma Ada_2022;
--  ===========================================================================
--  Domain.Value_Object.Zone_Id
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Zone Id value object - immutable domain data.
--
--  ===========================================================================

package body Domain.Value_Object.Zone_Id is

   --  ========================================================================
   --  Query Operations
   --  ========================================================================

   function To_String (Zone_ID : Zone_Id_Type) return String is
      use Zone_Id_Strings;
   begin
      return To_String (Zone_ID.ID);
   end To_String;

   function Length (Zone_ID : Zone_Id_Type) return Natural is
      use Zone_Id_Strings;
   begin
      return Zone_Id_Strings.Length (Zone_ID.ID);
   end Length;

   function Is_Empty (Zone_ID : Zone_Id_Type) return Boolean is
   begin
      return Length (Zone_ID) = 0;
   end Is_Empty;

   --  ========================================================================
   --  Operators
   --  ========================================================================

   overriding function "=" (Left, Right : Zone_Id_Type) return Boolean is
      use Zone_Id_Strings;
   begin
      return Left.ID = Right.ID;
   end "=";

   function "<" (Left, Right : Zone_Id_Type) return Boolean is
      use Zone_Id_Strings;
   begin
      return Left.ID < Right.ID;
   end "<";

end Domain.Value_Object.Zone_Id;
