pragma Ada_2022;
--  ===========================================================================
--  Infrastructure.Adapter.Discovery - TZif Discovery Adapter
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Concrete adapter for Discovery operations using the TZif library.
--    Converts between Domain types and TZif types to maintain DIP compliance.
--
--  Architecture Notes:
--    - ADAPTER: Converts Domain types ↔ TZif types
--    - TZif is PRIVATE - not visible to clients
--    - Public interface uses only Domain types
--    - All TZif access is encapsulated in the body
--    - Returns bounded arrays for SPARK-compatible zone listing
--
--  DIP Compliance:
--    - High-level modules (API, Application) depend on Domain abstractions
--    - This adapter implements the conversion to concrete TZif
--    - TZif can be swapped without changing public interface
--
--  Platform:
--    - Desktop/server environments with filesystem access
--    - Requires access to tzdata directories
--
--  See Also:
--    Domain.Value_Object.Source_Info - Domain source types
--    Domain.Value_Object.Zone_ID - Domain zone ID type and bounded arrays
--    Application.Usecase.Discovery - Use case that uses this adapter
--  ===========================================================================

pragma SPARK_Mode (Off);  --  Uses TZif which has I/O

with Domain.Value_Object.Source_Info;
with Domain.Value_Object.Zone_ID;
with Domain.Error;
with Domain.Error.Result;
with Domain.Error.Unit_Result;

package Infrastructure.Adapter.Discovery is

   --  ========================================================================
   --  Re-export Domain Types (NOT TZif types)
   --  ========================================================================

   --  Source Info types from Domain
   subtype Source_Info is Domain.Value_Object.Source_Info.Source_Info;
   subtype ULID_Type is Domain.Value_Object.Source_Info.ULID_Type;
   subtype Path_String is Domain.Value_Object.Source_Info.Path_String;
   subtype Path_List is Domain.Value_Object.Source_Info.Path_List;
   subtype Version_String is Domain.Value_Object.Source_Info.Version_String;

   --  Bounded string packages for construction
   package Path_Strings renames Domain.Value_Object.Source_Info.Path_Strings;
   package Version_Strings renames
     Domain.Value_Object.Source_Info.Version_Strings;

   --  Zone ID and list types from Domain
   subtype Zone_ID is Domain.Value_Object.Zone_ID.Zone_ID;
   subtype Zone_List is Domain.Value_Object.Zone_ID.Zone_List;
   subtype Search_Results is Domain.Value_Object.Zone_ID.Search_Results;

   package Zone_ID_Result renames Domain.Value_Object.Zone_ID.Zone_ID_Result;
   package Zone_List_Result renames
     Domain.Value_Object.Zone_ID.Zone_List_Result;
   package Search_Results_Result renames
     Domain.Value_Object.Zone_ID.Search_Results_Result;

   --  Error types from Domain
   subtype Error_Type is Domain.Error.Error_Type;
   subtype Error_Kind is Domain.Error.Error_Kind;
   package Error_Strings renames Domain.Error.Error_Strings;

   --  ========================================================================
   --  Result Types (instantiated with Domain types)
   --  ========================================================================

   --  Suppress unused visibility warnings from Generic_Result instantiation.
   --  Error_Strings is used in the generic body but GNAT warns when
   --  instantiation doesn't exercise all code paths using it.
   --  Note: pragma is required here - aspects don't apply to instantiations.
   pragma Warnings (Off, "no entities*");

   package Source_Info_Result is new Domain.Error.Result.Generic_Result
     (T => Source_Info);

   package Version_Result is new Domain.Error.Result.Generic_Result
     (T => Version_String);

   pragma Warnings (On, "no entities*");

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
   --  Platform-specific: reads /etc/localtime on POSIX,
   --  queries Windows API on Windows 10+.
   function Find_My_Id return Zone_ID_Result.Result;

   --  Get the version of a timezone database source.
   function Get_Version
     (Source : Source_Info) return Version_Result.Result;

   --  List all available timezone IDs from a source.
   --  Returns Zone_List or Overflow_Error if exceeds Max_Zone_List_Size.
   function List_All_Zones
     (Source     : Source_Info;
      Descending : Boolean := False) return Zone_List_Result.Result;

   --  ========================================================================
   --  Pattern-Based Search Operations
   --  ========================================================================

   --  Find zones matching a substring pattern.
   --  Returns bounded Search_Results or Overflow_Error if exceeds capacity.
   --  Example: Pattern "York" matches "America/New_York"
   function Find_By_Pattern
     (Pattern : String) return Search_Results_Result.Result;

   --  Find zones in a geographic region.
   --  Region is the first component of the IANA zone ID.
   --  Example: Region "America" matches "America/New_York", "America/Chicago"
   --  Returns bounded Search_Results or Overflow_Error if exceeds capacity.
   function Find_By_Region
     (Region : String) return Search_Results_Result.Result;

   --  Find zones matching a regular expression.
   --  Uses GNAT.Regpat for pattern matching.
   --  Returns bounded Search_Results or Overflow_Error if exceeds capacity.
   function Find_By_Regex
     (Regex : String) return Search_Results_Result.Result;

   --  ========================================================================
   --  Convenience Functions
   --  ========================================================================

   --  Convert Zone_ID to String
   function To_String (Id : Zone_ID) return String
     renames Domain.Value_Object.Zone_ID.To_String;

end Infrastructure.Adapter.Discovery;
