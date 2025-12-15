pragma Ada_2022;
--  =========================================================================
--  Domain.Value_Object.Zone_ID - Implementation
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--  =========================================================================

with Domain.Error;
use Domain.Error;

package body Domain.Value_Object.Zone_ID is

   --  Internal UTC string constant
   UTC_String : constant String := "UTC";

   --  ========================================================================
   --  Constants
   --  ========================================================================

   function UTC return Zone_ID is
   begin
      return
        (Value => Zone_ID_Strings.To_Bounded_String (UTC_String));
   end UTC;

   --  ========================================================================
   --  Smart Constructor
   --  ========================================================================

   function From_String (S : String) return Zone_ID_Result.Result is
   begin
      --  Validate: not empty
      if S'Length = 0 then
         return Zone_ID_Result.Error
           (Kind    => Validation_Error,
            Message => "Zone_ID cannot be empty");
      end if;

      --  Validate: not too long
      if S'Length > Max_Zone_ID_Length then
         return Zone_ID_Result.Error
           (Kind    => Validation_Error,
            Message => "Zone_ID exceeds maximum length");
      end if;

      --  Create the Zone_ID
      return Zone_ID_Result.Ok
        ((Value => Zone_ID_Strings.To_Bounded_String (S)));
   end From_String;

   --  ========================================================================
   --  Make_Zone_ID (unchecked constructor)
   --  ========================================================================

   function Make_Zone_ID (S : String) return Zone_ID is
   begin
      return (Value => Zone_ID_Strings.To_Bounded_String (S));
   end Make_Zone_ID;

   --  ========================================================================
   --  Accessors
   --  ========================================================================

   function To_String (Self : Zone_ID) return String is
   begin
      return Zone_ID_Strings.To_String (Self.Value);
   end To_String;

   --  ========================================================================
   --  Queries
   --  ========================================================================

   function Is_UTC (Self : Zone_ID) return Boolean is
   begin
      return Zone_ID_Strings.To_String (Self.Value) = UTC_String;
   end Is_UTC;

   function Is_Valid (Self : Zone_ID) return Boolean is
   begin
      return Zone_ID_Strings.Length (Self.Value) > 0;
   end Is_Valid;

   --  ========================================================================
   --  Comparison
   --  ========================================================================

   overriding function "=" (Left, Right : Zone_ID) return Boolean is
   begin
      return Zone_ID_Strings.To_String (Left.Value) =
             Zone_ID_Strings.To_String (Right.Value);
   end "=";

end Domain.Value_Object.Zone_ID;
