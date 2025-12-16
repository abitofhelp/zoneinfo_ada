pragma Ada_2022;
--  ==========================================================================
--  Zoneinfo_Config - STM32MP135F-DK Profile (Linux)
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Configuration for STM32MP135F-DK running Linux (OpenSTLinux).
--    Server-class configuration with generous memory allocation.
--
--  Target Hardware:
--    - STM32MP135FAF7 (Cortex-A7 @ 1 GHz) - Microprocessor (MPU)
--    - External DDR3L: 4 Gbit (512 MB)
--    - Operating System: Linux (OpenSTLinux distribution)
--
--  Design Philosophy:
--    Server/desktop-class configuration:
--    - No memory constraints (512 MB RAM)
--    - Maximum compatibility
--
--  Use Cases:
--    - IoT gateway
--    - Embedded Linux server
--    - Development and testing platform
--  ==========================================================================

package Zoneinfo_Config is

   pragma Pure;

   --  =======================================================================
   --  Profile Metadata
   --  =======================================================================

   Profile_Name     : constant String := "stm32mp135_linux";
   Target_Platform  : constant String := "STM32MP135F-DK (Linux MPU)";
   Target_RAM_KB    : constant Positive := 524_288;  -- 512 MB
   Operating_System : constant String := "Linux (OpenSTLinux)";

   --  =======================================================================
   --  Build Profile (Alire standard)
   --  =======================================================================

   type Build_Profile_Kind is (release, validation, development);
   Build_Profile : constant Build_Profile_Kind := development;

   --  =======================================================================
   --  Bounded String Configuration
   --  =======================================================================

   --  Maximum length of error messages
   --  Linux MPU profile: 512 characters
   Max_Error_Length : constant := 512;

   --  Maximum length of IANA timezone identifiers (e.g., "America/New_York")
   --  Longest known: ~30 characters; 64 provides margin
   Max_Zone_ID_Length : constant := 64;

   --  Maximum length of formatted datetime strings (ISO 8601 with zone)
   --  Example: "2025-12-15T14:30:00.123456789-05:00[America/New_York]"
   Max_Datetime_Length : constant := 96;

   --  Maximum length of formatted duration strings (ISO 8601)
   --  Example: "P1Y2M3DT4H5M6.789S"
   Max_Duration_Length : constant := 48;

   --  =======================================================================
   --  Discovery Configuration
   --  =======================================================================

   --  Maximum length of filesystem paths
   --  Linux MPU profile: 4096 characters
   Max_Path_Length : constant := 4_096;

   --  Maximum length of timezone database version strings
   --  Linux MPU profile: 32 characters
   Max_Version_Length : constant := 32;

   --  Maximum search paths for source discovery
   --  Linux MPU profile: 100 paths
   Max_Search_Paths : constant := 100;

   --  Maximum zones returned by List_All_Zones
   --  Linux MPU: full desktop capability
   Max_Zone_List_Size : constant := 750;

   --  Maximum zones returned by Find_By_* search operations
   --  Linux MPU profile: generous result sets
   Max_Search_Results : constant := 100;

   --  =======================================================================
   --  Memory Planning Constants
   --  =======================================================================

   --  Bounded string overhead (length field + alignment, platform estimate)
   Bounded_String_Overhead : constant := 8;

   --  Bytes per Zone_ID (for memory planning)
   Zone_ID_Size_Bytes : constant := Max_Zone_ID_Length + Bounded_String_Overhead;

   --  Memory estimates (bytes)
   Zone_List_Memory_Bytes : constant :=
     Max_Zone_List_Size * Zone_ID_Size_Bytes;  --  ~54KB
   Search_Results_Memory_Bytes : constant :=
     Max_Search_Results * Zone_ID_Size_Bytes;  --  ~7.2KB

end Zoneinfo_Config;
