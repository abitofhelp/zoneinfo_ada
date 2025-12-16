pragma Ada_2022;
--  ==========================================================================
--  Zoneinfo_Config - STM32H7S78-DK Profile
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Configuration optimized for STM32H7S78-DK Discovery Kit.
--    Leverages 620KB internal SRAM + 32MB external PSRAM.
--
--  Target Hardware:
--    - STM32H7S7L8H6H (Cortex-M7 @ 600 MHz)
--    - Internal SRAM: 620 KB
--    - External PSRAM: 256 Mbit (32 MB)
--    - External Flash: 1 Gbit (128 MB)
--
--  Design Philosophy:
--    High-performance embedded with generous external RAM.
--    Can afford larger string buffers than typical embedded.
--  ==========================================================================

package Zoneinfo_Config is

   pragma Pure;

   --  =======================================================================
   --  Profile Metadata
   --  =======================================================================

   Profile_Name      : constant String := "stm32h7s78";
   Target_Platform   : constant String := "STM32H7S78-DK";
   Target_RAM_KB     : constant Positive := 620;
   External_PSRAM_MB : constant Positive := 32;

   --  =======================================================================
   --  Build Profile (Alire standard)
   --  =======================================================================

   type Build_Profile_Kind is (release, validation, development);
   Build_Profile : constant Build_Profile_Kind := development;

   --  =======================================================================
   --  Bounded String Configuration
   --  =======================================================================

   --  Maximum length of error messages
   --  STM32H7S78 profile: 256 characters
   Max_Error_Length : constant := 256;

   --  Maximum length of IANA timezone identifiers (e.g., "America/New_York")
   --  Fixed size - IANA format constraint
   Max_Zone_ID_Length : constant := 64;

   --  Maximum length of formatted datetime strings (ISO 8601 with zone)
   --  STM32H7S78 profile: 64 characters (external PSRAM allows generous sizing)
   Max_Datetime_Length : constant := 64;

   --  Maximum length of formatted duration strings (ISO 8601)
   --  STM32H7S78 profile: 32 characters
   Max_Duration_Length : constant := 32;

   --  =======================================================================
   --  Discovery Configuration
   --  =======================================================================

   --  Maximum length of filesystem paths
   --  STM32H7S78 profile: 512 characters
   Max_Path_Length : constant := 512;

   --  Maximum length of timezone database version strings
   --  STM32H7S78 profile: 16 characters
   Max_Version_Length : constant := 16;

   --  Maximum search paths for source discovery
   --  STM32H7S78 profile: 20 paths
   Max_Search_Paths : constant := 20;

   --  Maximum zones returned by List_All_Zones
   --  STM32H7S78: external PSRAM allows moderate capacity
   Max_Zone_List_Size : constant := 100;

   --  Maximum zones returned by Find_By_* search operations
   --  STM32H7S78 profile: moderate result sets
   Max_Search_Results : constant := 30;

   --  =======================================================================
   --  Memory Planning Constants
   --  =======================================================================

   --  Bounded string overhead (length field + alignment, platform estimate)
   Bounded_String_Overhead : constant := 8;

   --  Bytes per Zone_ID (for memory planning)
   Zone_ID_Size_Bytes : constant := Max_Zone_ID_Length + Bounded_String_Overhead;

   --  Memory estimates (bytes)
   Zone_List_Memory_Bytes : constant :=
     Max_Zone_List_Size * Zone_ID_Size_Bytes;  --  ~7.2KB
   Search_Results_Memory_Bytes : constant :=
     Max_Search_Results * Zone_ID_Size_Bytes;  --  ~2.2KB

end Zoneinfo_Config;
