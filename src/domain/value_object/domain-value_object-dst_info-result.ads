pragma Ada_2022;
--  ===========================================================================
--  Domain.Value_Object.Dst_Info.Result
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

with Domain.Error.Result;

package Domain.Value_Object.DST_Info.Result is

   pragma Elaborate_Body;

   --  Instantiate Result monad for Ambiguity_Info_Type
   package Ambiguity_Info_Result_Package is new
     Domain.Error.Result.Generic_Result (T => Ambiguity_Info_Type);

   subtype Result is Ambiguity_Info_Result_Package.Result;

   --  Re-export common operations
   function Is_Ok (R : Result) return Boolean renames
     Ambiguity_Info_Result_Package.Is_Ok;

   function Is_Error (R : Result) return Boolean renames
     Ambiguity_Info_Result_Package.Is_Error;

   function Value (R : Result) return Ambiguity_Info_Type renames
     Ambiguity_Info_Result_Package.Value;

   function Error (Kind : Domain.Error.Error_Kind; Message : String) return Result renames
     Ambiguity_Info_Result_Package.Error;

   function Ok (Val : Ambiguity_Info_Type) return Result renames
     Ambiguity_Info_Result_Package.Ok;

end Domain.Value_Object.DST_Info.Result;
