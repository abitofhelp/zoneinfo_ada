pragma Ada_2022;
--  ===========================================================================
--  Application.Usecase.Discovery - Timezone Discovery Use Case
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Generic use case for timezone source discovery operations. Parameterized
--    by discovery adapter functions to enable dependency injection.
--
--  Architecture Notes:
--    - GENERIC USE CASE: Takes Discovery adapter functions as formals
--    - Static dispatch: No runtime overhead for port calls
--    - Testable: Instantiate with mock adapters for deterministic tests
--    - Uses DOMAIN TYPES throughout - no TZif type leakage
--
--  Usage:
--    --  In API.Discovery (composition root):
--    package Discovery_UC is new Application.Usecase.Discovery
--      (Discover_Sources => Infra.Adapter.Discovery.Discover_Sources,
--       Load_Source      => Infra.Adapter.Discovery.Load_Source,
--       ...);
--
--    --  Then call:
--    Result := Discovery_UC.Discover_Sources (Paths);
--
--  See Also:
--    Infrastructure.Adapter.Discovery - Production discovery adapter
--    Domain.Value_Object.Source_Info - Source_Info type
--    Domain.Value_Object.Zone_ID - Zone_ID type
--  ===========================================================================

with Domain.Value_Object.Source_Info;
with Domain.Value_Object.Zone_ID;
with Domain.Error.Result;
with Domain.Error.Unit_Result;

generic
   --  ========================================================================
   --  Port Function Parameters (all use Domain types)
   --  ========================================================================

   --  Result packages from adapter
   with package Source_Info_Result is new Domain.Error.Result.Generic_Result
     (T => Domain.Value_Object.Source_Info.Source_Info);
   with package Version_Result is new Domain.Error.Result.Generic_Result
     (T => Domain.Value_Object.Source_Info.Version_String);

   --  Zone callback type for iteration
   type Zone_Callback is access procedure
     (Zone : Domain.Value_Object.Zone_ID.Zone_ID);

   --  Source Management Operations
   with function Port_Discover_Sources
     (Search_Paths : Domain.Value_Object.Source_Info.Path_List)
      return Source_Info_Result.Result;
   with function Port_Load_Source
     (Path : Domain.Value_Object.Source_Info.Path_String)
      return Source_Info_Result.Result;
   with function Port_Validate_Source
     (Path : Domain.Value_Object.Source_Info.Path_String)
      return Domain.Error.Unit_Result.Result;

   --  Timezone Query Operations
   with function Port_Find_My_Id
     return Domain.Value_Object.Zone_ID.Zone_ID_Result.Result;
   with function Port_Get_Version
     (Source : Domain.Value_Object.Source_Info.Source_Info)
      return Version_Result.Result;
   with function Port_List_All_Zones
     (Source     : Domain.Value_Object.Source_Info.Source_Info;
      Yield      : Zone_Callback;
      Descending : Boolean := False)
      return Domain.Error.Unit_Result.Result;

   --  Pattern-Based Search Operations
   with function Port_Find_By_Pattern
     (Pattern : String;
      Yield   : Zone_Callback)
      return Domain.Error.Unit_Result.Result;
   with function Port_Find_By_Region
     (Region : String;
      Yield  : Zone_Callback)
      return Domain.Error.Unit_Result.Result;
   with function Port_Find_By_Regex
     (Regex : String;
      Yield : Zone_Callback)
      return Domain.Error.Unit_Result.Result;

package Application.Usecase.Discovery is

   --  ========================================================================
   --  Re-export Domain Types for convenience
   --  ========================================================================

   subtype Source_Info is Domain.Value_Object.Source_Info.Source_Info;
   subtype Path_String is Domain.Value_Object.Source_Info.Path_String;
   subtype Path_List is Domain.Value_Object.Source_Info.Path_List;
   subtype Version_String is Domain.Value_Object.Source_Info.Version_String;
   subtype Zone_ID is Domain.Value_Object.Zone_ID.Zone_ID;

   package Path_Strings renames Domain.Value_Object.Source_Info.Path_Strings;
   package Version_Strings renames
     Domain.Value_Object.Source_Info.Version_Strings;
   package Zone_ID_Result renames Domain.Value_Object.Zone_ID.Zone_ID_Result;
   package Unit_Result renames Domain.Error.Unit_Result;

   --  ========================================================================
   --  Source Management Operations
   --  ========================================================================

   --  Discover timezone sources from given paths.
   --  Scans directories for valid TZif data sources.
   function Discover_Sources
     (Search_Paths : Path_List) return Source_Info_Result.Result;

   --  Load a timezone source from a path.
   --  Returns Source_Info on success.
   function Load_Source
     (Path : Path_String) return Source_Info_Result.Result;

   --  Validate a timezone source at the given path.
   --  Checks for required files and structure.
   function Validate_Source
     (Path : Path_String) return Unit_Result.Result;

   --  ========================================================================
   --  Timezone Query Operations
   --  ========================================================================

   --  Get the local system timezone.
   function Find_My_Id return Zone_ID_Result.Result;

   --  Get the version of a timezone database source.
   function Get_Version
     (Source : Source_Info) return Version_Result.Result;

   --  List all available timezone IDs from a source.
   function List_All_Zones
     (Source     : Source_Info;
      Yield      : Zone_Callback;
      Descending : Boolean := False) return Unit_Result.Result;

   --  ========================================================================
   --  Pattern-Based Search Operations
   --  ========================================================================

   --  Find zones matching a substring pattern.
   function Find_By_Pattern
     (Pattern : String;
      Yield   : Zone_Callback) return Unit_Result.Result;

   --  Find zones in a geographic region.
   function Find_By_Region
     (Region : String;
      Yield  : Zone_Callback) return Unit_Result.Result;

   --  Find zones matching a regular expression.
   function Find_By_Regex
     (Regex : String;
      Yield : Zone_Callback) return Unit_Result.Result;

   --  ========================================================================
   --  Convenience Functions
   --  ========================================================================

   function To_String (Id : Zone_ID) return String
     renames Domain.Value_Object.Zone_ID.To_String;

end Application.Usecase.Discovery;
