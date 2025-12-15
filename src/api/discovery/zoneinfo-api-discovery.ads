pragma Ada_2022;
--  ===========================================================================
--  Zoneinfo.API.Discovery - Timezone Source Discovery Composition Root
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Composition root for timezone source discovery operations. Wires the
--    Infrastructure.Adapter.Discovery to Application.Usecase.Discovery
--    to provide ready-to-use discovery operations.
--
--  Architecture:
--    - COMPOSITION ROOT for Discovery operations
--    - SPARK_Mode Off: Contains I/O wiring
--    - Uses DOMAIN TYPES throughout - no TZif type exposure
--    - Provides convenient API for source management and timezone queries
--
--  DIP Compliance:
--    - All public types are from Domain layer (Source_Info, Zone_ID, etc.)
--    - TZif is fully encapsulated in Infrastructure.Adapter.Discovery
--    - Clients depend only on Domain abstractions
--
--  Usage:
--    with Zoneinfo.API.Discovery;
--    use Zoneinfo.API.Discovery;
--
--    --  Discover timezone sources
--    Paths : Path_List (1 .. 2);
--    Paths (1) := Make_Path ("/usr/share/zoneinfo");
--    Paths (2) := Make_Path ("/var/db/timezone/zoneinfo");
--    Result := Discover_Sources (Paths);
--
--  ===========================================================================

pragma SPARK_Mode (Off);

with Application.Usecase.Discovery;
with Infrastructure.Adapter.Discovery;
with Domain.Value_Object.Source_Info;
with Domain.Value_Object.Zone_ID;
with Domain.Error;
with Domain.Error.Unit_Result;

package Zoneinfo.API.Discovery is

   --  ========================================================================
   --  Re-export Domain Types (NOT TZif types)
   --  ========================================================================

   package Adapter renames Infrastructure.Adapter.Discovery;

   --  Zone ID types from Domain
   subtype Zone_ID is Domain.Value_Object.Zone_ID.Zone_ID;
   package Zone_ID_Result renames Domain.Value_Object.Zone_ID.Zone_ID_Result;

   --  Error types from Domain
   subtype Error_Type is Domain.Error.Error_Type;
   subtype Error_Kind is Domain.Error.Error_Kind;
   package Error_Strings renames Domain.Error.Error_Strings;

   --  Source management types from Domain
   subtype Source_Info is Domain.Value_Object.Source_Info.Source_Info;
   subtype Path_String is Domain.Value_Object.Source_Info.Path_String;
   subtype Path_List is Domain.Value_Object.Source_Info.Path_List;
   subtype Version_String is Domain.Value_Object.Source_Info.Version_String;

   --  Bounded string packages for construction
   package Path_Strings renames Domain.Value_Object.Source_Info.Path_Strings;
   package Version_Strings renames
     Domain.Value_Object.Source_Info.Version_Strings;

   --  Result packages from Adapter
   package Source_Info_Result renames Adapter.Source_Info_Result;
   package Version_Result renames Adapter.Version_Result;
   package Unit_Result renames Domain.Error.Unit_Result;

   --  Zone callback type for iteration
   subtype Zone_Callback is Adapter.Zone_Callback;

   --  ========================================================================
   --  Convenience Constructors
   --  ========================================================================

   function Make_Path (Value : String) return Path_String
     renames Domain.Value_Object.Source_Info.Make_Path;

   --  ========================================================================
   --  Wired Use Case (Internal)
   --  ========================================================================

   --  Discovery use case instantiated with Domain-based adapter
   package Discovery_UC is new Application.Usecase.Discovery
     (Source_Info_Result    => Adapter.Source_Info_Result,
      Version_Result        => Adapter.Version_Result,
      Zone_Callback         => Adapter.Zone_Callback,
      Port_Discover_Sources => Adapter.Discover_Sources,
      Port_Load_Source      => Adapter.Load_Source,
      Port_Validate_Source  => Adapter.Validate_Source,
      Port_Find_My_Id       => Adapter.Find_My_Id,
      Port_Get_Version      => Adapter.Get_Version,
      Port_List_All_Zones   => Adapter.List_All_Zones,
      Port_Find_By_Pattern  => Adapter.Find_By_Pattern,
      Port_Find_By_Region   => Adapter.Find_By_Region,
      Port_Find_By_Regex    => Adapter.Find_By_Regex);

   --  ========================================================================
   --  Source Management Operations
   --  ========================================================================

   --  Discover timezone sources from given paths.
   --  Scans directories for valid TZif data sources.
   function Discover_Sources
     (Search_Paths : Path_List) return Source_Info_Result.Result
     renames Discovery_UC.Discover_Sources;

   --  Load a timezone source from a path.
   --  Returns Source_Info on success.
   function Load_Source
     (Path : Path_String) return Source_Info_Result.Result
     renames Discovery_UC.Load_Source;

   --  Validate a timezone source at the given path.
   --  Checks for required files and structure.
   function Validate_Source
     (Path : Path_String) return Unit_Result.Result
     renames Discovery_UC.Validate_Source;

   --  ========================================================================
   --  Timezone Query Operations
   --  ========================================================================

   --  Get the local system timezone.
   --  Platform-specific: reads /etc/localtime on POSIX,
   --  queries Windows API on Windows 10+.
   function Find_My_Id return Zone_ID_Result.Result
     renames Discovery_UC.Find_My_Id;

   --  Get the version of a timezone database source.
   function Get_Version
     (Source : Source_Info) return Version_Result.Result
     renames Discovery_UC.Get_Version;

   --  List all available timezone IDs from a source.
   --  Calls Yield callback for each zone ID.
   function List_All_Zones
     (Source     : Source_Info;
      Yield      : Zone_Callback;
      Descending : Boolean := False) return Unit_Result.Result
     renames Discovery_UC.List_All_Zones;

   --  ========================================================================
   --  Pattern-Based Search Operations
   --  ========================================================================

   --  Find zones matching a substring pattern.
   --  Calls Yield callback for each matching zone ID.
   --  Example: Pattern "York" matches "America/New_York"
   function Find_By_Pattern
     (Pattern : String;
      Yield   : Zone_Callback) return Unit_Result.Result
     renames Discovery_UC.Find_By_Pattern;

   --  Find zones in a geographic region.
   --  Region is the first component of the IANA zone ID.
   --  Example: Region "America" matches "America/New_York", "America/Chicago"
   function Find_By_Region
     (Region : String;
      Yield  : Zone_Callback) return Unit_Result.Result
     renames Discovery_UC.Find_By_Region;

   --  Find zones matching a regular expression.
   --  Uses GNAT.Regpat for pattern matching.
   function Find_By_Regex
     (Regex : String;
      Yield : Zone_Callback) return Unit_Result.Result
     renames Discovery_UC.Find_By_Regex;

   --  ========================================================================
   --  Result Query Helpers
   --  ========================================================================

   --  Zone_ID_Result helpers
   function Is_Ok (R : Zone_ID_Result.Result) return Boolean
     renames Zone_ID_Result.Is_Ok;

   function Is_Error (R : Zone_ID_Result.Result) return Boolean
     renames Zone_ID_Result.Is_Error;

   function Value (R : Zone_ID_Result.Result) return Zone_ID
     renames Zone_ID_Result.Value;

   --  Source_Info_Result helpers
   function Is_Ok (R : Source_Info_Result.Result) return Boolean
     renames Source_Info_Result.Is_Ok;

   function Is_Error (R : Source_Info_Result.Result) return Boolean
     renames Source_Info_Result.Is_Error;

   function Value (R : Source_Info_Result.Result) return Source_Info
     renames Source_Info_Result.Value;

   --  ========================================================================
   --  Convenience Functions
   --  ========================================================================

   --  Convert Zone_ID to String
   function To_String (Id : Zone_ID) return String
     renames Domain.Value_Object.Zone_ID.To_String;

end Zoneinfo.API.Discovery;
