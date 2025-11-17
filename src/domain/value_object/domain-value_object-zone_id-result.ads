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
--  Responsibilities:
--    - Define Result type and operations
--    - Provide constructors and accessors
--    - Validate domain constraints
--
--  Key Types:
--    Result
--
--  ===========================================================================

with Domain.Error;
with Domain.Error.Result;

package Domain.Value_Object.Zone_Id.Result is

   pragma Elaborate_Body;

   --  ========================================================================
   --  Specialized Result Type for Zone_Id
   --  ========================================================================

   package Impl is new Domain.Error.Result.Generic_Result
     (T => Domain.Value_Object.Zone_Id.Zone_Id_Type);

   --  Public alias for the specialized Result type
   subtype Result is Impl.Result;

   --  Re-export core Result operations for convenience

   function Ok
     (Value : Domain.Value_Object.Zone_Id.Zone_Id_Type)
      return Result
   renames Impl.Ok;

   function Error
     (Kind    : Domain.Error.Error_Kind;
      Message : String)
      return Result
   renames Impl.Error;

   function Is_Ok (Self : Result) return Boolean
     renames Impl.Is_Ok;

   function Is_Error (Self : Result) return Boolean
     renames Impl.Is_Error;

   function Value (Self : Result)
      return Domain.Value_Object.Zone_Id.Zone_Id_Type
   renames Impl.Value;

   function Error_Info (Self : Result)
      return Domain.Error.Error_Type
   renames Impl.Error_Info;

   --  ========================================================================
   --  Smart Constructor
   --  ========================================================================

   --  Create a Zone_Id from string with validation
   --  Returns Error if:
   --    - Empty string
   --    - Length > Max_Zone_Id_Length
   --    - Invalid characters (only allows: a-z, A-Z, 0-9, _, -, /, +)
   function Create (ID : String) return Result;

end Domain.Value_Object.Zone_Id.Result;
