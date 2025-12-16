pragma Ada_2022;
--  ==========================================================================
--  Zoneinfo_Config - Embedded Profile
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Configuration profile for Ravenscar-compatible embedded systems.
--    Balanced configuration for memory-constrained devices.
--
--  Target Hardware:
--    - STM32F769 or similar (Cortex-M7 @ 200+ MHz)
--    - RAM: 512KB - 1MB
--    - Ravenscar runtime
--
--  Design Philosophy:
--    Conservative sizing with safety margins for IoT devices and
--    industrial control systems.
--  ==========================================================================

package Zoneinfo_Config is

   pragma Pure;

   --  =======================================================================
   --  Profile Metadata
   --  =======================================================================

   Profile_Name    : constant String := "embedded";
   Target_Platform : constant String := "Embedded (Ravenscar)";

   --  =======================================================================
   --  Build Profile (Alire standard)
   --  =======================================================================

   type Build_Profile_Kind is (release, validation, development);
   Build_Profile : constant Build_Profile_Kind := release;

   --  =======================================================================
   --  Bounded String Configuration
   --  =======================================================================

   --  Maximum length of error messages
   --  Embedded profile: 256 characters
   Max_Error_Length : constant := 256;

   --  Maximum length of IANA timezone identifiers (e.g., "America/New_York")
   --  Fixed size - IANA format constraint
   Max_Zone_ID_Length : constant := 64;

   --  Maximum length of formatted datetime strings (ISO 8601 with zone)
   --  Embedded profile: 64 characters (shorter formats)
   Max_Datetime_Length : constant := 64;

   --  Maximum length of formatted duration strings (ISO 8601)
   --  Embedded profile: 32 characters
   Max_Duration_Length : constant := 32;

   --  =======================================================================
   --  Discovery Configuration
   --  =======================================================================

   --  Maximum length of filesystem paths
   --  Embedded profile: 256 characters (shorter paths typical)
   Max_Path_Length : constant := 256;

   --  Maximum length of timezone database version strings
   --  Embedded profile: 16 characters
   Max_Version_Length : constant := 16;

   --  Maximum search paths for source discovery
   --  Embedded profile: 10 paths
   Max_Search_Paths : constant := 10;

   --  Maximum zones returned by List_All_Zones
   --  Embedded apps typically use limited set of zones
   Max_Zone_List_Size : constant := 50;

   --  Maximum zones returned by Find_By_* search operations
   --  Embedded profile: smaller result sets
   Max_Search_Results : constant := 20;

   --  =======================================================================
   --  Memory Planning Constants
   --  =======================================================================

   --  Bounded string overhead (length field + alignment, platform estimate)
   Bounded_String_Overhead : constant := 8;

   --  Bytes per Zone_ID (for memory planning)
   Zone_ID_Size_Bytes : constant := Max_Zone_ID_Length + Bounded_String_Overhead;

   --  Memory estimates (bytes)
   Zone_List_Memory_Bytes : constant :=
     Max_Zone_List_Size * Zone_ID_Size_Bytes;  --  ~3.6KB
   Search_Results_Memory_Bytes : constant :=
     Max_Search_Results * Zone_ID_Size_Bytes;  --  ~1.4KB

end Zoneinfo_Config;
