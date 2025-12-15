pragma Ada_2022;
--  ===========================================================================
--  Domain.Value_Object.Source_Info - Timezone Source Information
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Source Info value object - immutable domain data representing a
--    timezone data source (e.g., a directory containing TZif files).
--
--  Responsibilities:
--    - Define Source Info type and operations
--    - Provide constructors and accessors
--    - Define Path and Version string types
--
--  Key Types:
--    ULID_Type          - Unique identifier for source
--    Version_String     - Database version (e.g., "2024b")
--    Path_String        - Filesystem path to source
--    Source_Info        - Complete source information record
--
--  Dependencies:
--    Zoneinfo_Config
--    Domain.Error.Result
--    Preelaborate
--
--  ===========================================================================

with Ada.Strings;
with Ada.Strings.Bounded;
with Zoneinfo_Config;
with Domain.Error.Result;

package Domain.Value_Object.Source_Info with
  Preelaborate
is

   --  ========================================================================
   --  ULID Type (26 characters, Base32 encoded)
   --  ========================================================================
   --  ULIDs are lexicographically sortable, timestamp-based unique identifiers
   --  Format: 10 bytes timestamp + 16 bytes randomness = 26 Base32 characters
   --  ========================================================================

   ULID_Length : constant := 26;

   package ULID_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
     (ULID_Length);
   subtype ULID_Type is ULID_Strings.Bounded_String;

   --  Crockford's Base32 alphabet (for validation)
   Base32_Alphabet : constant String := "0123456789ABCDEFGHJKMNPQRSTVWXYZ";

   --  Null/zero ULID (all '0' characters)
   function Null_ULID return ULID_Type
     with Inline,
          Post => To_String (Null_ULID'Result) = [1 .. ULID_Length => '0'];

   --  Validate ULID string format
   function Is_Valid_ULID_String (S : String) return Boolean
     with Post =>
       (if Is_Valid_ULID_String'Result then
          S'Length = ULID_Length
          and then
          (for all C of S =>
             (for some Valid of Base32_Alphabet => C = Valid)));

   --  Unsafe constructor (precondition enforces validity)
   function Make_ULID (Value : String) return ULID_Type
     with Pre => Is_Valid_ULID_String (Value);

   --  Safe ULID parser with Result monad
   package ULID_Result is new Domain.Error.Result.Generic_Result
     (T => ULID_Type);

   function Parse_ULID (S : String) return ULID_Result.Result
     with Post =>
       (if ULID_Result.Is_Ok (Parse_ULID'Result) then
          not Is_Null (ULID_Result.Value (Parse_ULID'Result)));

   function To_String (ULID : ULID_Type) return String is
     (ULID_Strings.To_String (ULID))
     with Post => To_String'Result'Length = ULID_Length,
          Inline;

   --  Check if ULID is null/zero
   function Is_Null (ID : ULID_Type) return Boolean
     with Inline;

   --  ULID comparison (lexicographic)
   function "=" (Left, Right : ULID_Type) return Boolean renames
     ULID_Strings."=";
   function "<" (Left, Right : ULID_Type) return Boolean renames
     ULID_Strings."<";

   --  ========================================================================
   --  Version String Type
   --  ========================================================================
   --  Timezone database version (e.g., "2024b", "2023c")
   --  ========================================================================

   package Version_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
     (Zoneinfo_Config.Max_Version_Length);
   subtype Version_String is Version_Strings.Bounded_String;

   function Make_Version (Value : String) return Version_String with
     Pre => Value'Length <= Zoneinfo_Config.Max_Version_Length;

   function To_String (Version : Version_String) return String is
     (Version_Strings.To_String (Version));

   --  ========================================================================
   --  Path String Type
   --  ========================================================================

   package Path_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
     (Zoneinfo_Config.Max_Path_Length);
   subtype Path_String is Path_Strings.Bounded_String;

   function Make_Path (Value : String) return Path_String with
     Pre => Value'Length > 0
            and then Value'Length <= Zoneinfo_Config.Max_Path_Length;

   function To_String (Path : Path_String) return String is
     (Path_Strings.To_String (Path));

   --  Path list for discovery operations
   type Path_List is array (Positive range <>) of Path_String;

   --  ========================================================================
   --  Source_Info - The Value Object
   --  ========================================================================

   type Source_Info is private;

   --  Default value (for SPARK-compatible bounded containers)
   function Default_Source_Info return Source_Info with
     Inline;

   --  Constructor
   function Make_Source_Info
     (ULID : ULID_Type; Path : Path_String; Version : Version_String;
      Zone_Count : Natural) return Source_Info;

   --  Accessors
   function Get_ULID (Source : Source_Info) return ULID_Type;
   function Get_Path (Source : Source_Info) return Path_String;
   function Get_Version (Source : Source_Info) return Version_String;
   function Get_Zone_Count (Source : Source_Info) return Natural;

   --  Comparison (by ULID)
   overriding function "=" (Left, Right : Source_Info) return Boolean;
   function "<" (Left, Right : Source_Info) return Boolean;

private

   type Source_Info is record
      ULID       : ULID_Type;
      Path       : Path_String;
      Version    : Version_String;
      Zone_Count : Natural;
   end record;

end Domain.Value_Object.Source_Info;
