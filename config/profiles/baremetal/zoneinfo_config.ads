pragma Ada_2022;
--  ==========================================================================
--  Zoneinfo_Config - Bare Metal Profile
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Configuration profile for Zero Footprint Profile (ZFP) systems.
--    Minimal configuration for bare metal microcontrollers.
--
--  Target Hardware:
--    - STM32F4xx or similar (Cortex-M4 @ 100+ MHz)
--    - RAM: 128KB - 256KB
--    - ZFP runtime (no OS, minimal stdlib)
--
--  Design Philosophy:
--    Minimal sizing for extremely constrained memory environments.
--    Every byte counts in ZFP systems.
--  ==========================================================================

package Zoneinfo_Config is

   pragma Pure;

   --  =======================================================================
   --  Profile Metadata
   --  =======================================================================

   Profile_Name    : constant String := "baremetal";
   Target_Platform : constant String := "Bare Metal (ZFP)";

   --  =======================================================================
   --  Build Profile (Alire standard)
   --  =======================================================================

   type Build_Profile_Kind is (release, validation, development);
   Build_Profile : constant Build_Profile_Kind := release;

   --  =======================================================================
   --  Bounded String Configuration
   --  =======================================================================

   --  Maximum length of error messages
   --  Bare metal profile: 128 characters
   Max_Error_Length : constant := 128;

   --  Maximum length of IANA timezone identifiers (e.g., "America/New_York")
   --  Fixed size - IANA format constraint
   Max_Zone_ID_Length : constant := 64;

   --  Maximum length of formatted datetime strings (ISO 8601)
   --  Bare metal profile: 48 characters (minimal formatting)
   Max_Datetime_Length : constant := 48;

   --  Maximum length of formatted duration strings (ISO 8601)
   --  Bare metal profile: 24 characters
   Max_Duration_Length : constant := 24;

   --  =======================================================================
   --  Discovery Configuration
   --  =======================================================================

   --  Maximum length of filesystem paths
   --  Bare metal profile: 128 characters (minimal)
   Max_Path_Length : constant := 128;

   --  Maximum length of timezone database version strings
   --  Bare metal profile: 16 characters
   Max_Version_Length : constant := 16;

   --  Maximum search paths for source discovery
   --  Bare metal profile: 3 paths
   Max_Search_Paths : constant := 3;

   --  Maximum zones returned by List_All_Zones
   --  Bare metal: minimal - typically single timezone
   Max_Zone_List_Size : constant := 10;

   --  Maximum zones returned by Find_By_* search operations
   --  Bare metal profile: minimal result sets
   Max_Search_Results : constant := 5;

   --  =======================================================================
   --  Memory Planning Constants
   --  =======================================================================

   --  Bounded string overhead (length field + alignment, platform estimate)
   Bounded_String_Overhead : constant := 8;

   --  Bytes per Zone_ID (for memory planning)
   Zone_ID_Size_Bytes : constant := Max_Zone_ID_Length + Bounded_String_Overhead;

   --  Memory estimates (bytes)
   Zone_List_Memory_Bytes : constant :=
     Max_Zone_List_Size * Zone_ID_Size_Bytes;  --  ~720 bytes
   Search_Results_Memory_Bytes : constant :=
     Max_Search_Results * Zone_ID_Size_Bytes;  --  ~360 bytes

end Zoneinfo_Config;
