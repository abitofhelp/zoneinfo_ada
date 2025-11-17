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
--  Responsibilities:
--    - Define Zone Id type and operations
--    - Provide constructors and accessors
--
--  Key Types:
--    Zone_Id_Type
--    Zone_Id_Type
--
--  Dependencies:
--    Post => To_String'Result'Length <= Max_Zone_Id_Length
--    Post => Length'Result <= Max_Zone_Id_Length
--    Pre => ID'Length > 0 and ID'Length <= Max_Zone_Id_Length
--
--  ===========================================================================

with Ada.Strings.Bounded;

package Domain.Value_Object.Zone_Id is

   pragma Elaborate_Body;

   --  ========================================================================
   --  Constants
   --  ========================================================================

   --  Maximum length for timezone identifier per IANA spec
   Max_Zone_Id_Length : constant := 64;

   --  ========================================================================
   --  Zone_Id Type
   --  ========================================================================

   --  Private type ensures validation through smart constructors.
   --  Smart constructors are defined in the child package
   --  Domain.Value_Object.Zone_Id.Result.
   type Zone_Id_Type is private;

   --  ========================================================================
   --  Query Operations
   --  ========================================================================

   --  Convert Zone_Id to String
   function To_String (Zone_ID : Zone_Id_Type) return String
   with Post => To_String'Result'Length <= Max_Zone_Id_Length;

   --  Get length of Zone_Id
   function Length (Zone_ID : Zone_Id_Type) return Natural
   with Post => Length'Result <= Max_Zone_Id_Length;

   --  Check if Zone_Id is empty (should never happen with valid construction)
   function Is_Empty (Zone_ID : Zone_Id_Type) return Boolean;

   --  ========================================================================
   --  Operators (FR-11.3: Intuitive usage)
   --  ========================================================================

   --  Equality operator
   overriding function "=" (Left, Right : Zone_Id_Type) return Boolean;

   --  Less than (for ordering/sorting)
   function "<" (Left, Right : Zone_Id_Type) return Boolean;

   --  ========================================================================
   --  Common Zone IDs (Constants for convenience)
   --  ========================================================================

   --  Note: These use a private constructor that bypasses validation
   --  since we know these are valid at compile time
   UTC : constant Zone_Id_Type;

private

   --  ========================================================================
   --  Private Implementation
   --  ========================================================================

   --  Bounded string for memory safety
   package Zone_Id_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
     (Max => Max_Zone_Id_Length);

   type Zone_Id_Type is record
      ID : Zone_Id_Strings.Bounded_String;
   end record;

   --  Private constructor for compile-time constants (no validation)
   function Make_Unchecked (ID : String) return Zone_Id_Type is
     (Zone_Id_Type'(ID => Zone_Id_Strings.To_Bounded_String (ID)))
   with Pre => ID'Length > 0 and ID'Length <= Max_Zone_Id_Length;

   --  Common timezone constants
   UTC : constant Zone_Id_Type := Make_Unchecked ("UTC");

end Domain.Value_Object.Zone_Id;
