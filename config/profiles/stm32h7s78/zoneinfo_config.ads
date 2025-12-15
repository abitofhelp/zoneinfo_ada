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

   --  Maximum length of person names
   --  STM32H7S78 profile: 64 characters (generous for embedded)
   Max_Name_Length : constant := 64;

   --  Maximum length of greeting messages
   --  STM32H7S78 profile: 128 characters
   Max_Message_Length : constant := 128;

   --  Maximum length of error messages
   --  STM32H7S78 profile: 256 characters
   Max_Error_Length : constant := 256;

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

   --  Maximum discovered timezone sources
   --  STM32H7S78 profile: 10 sources
   Max_Sources : constant := 10;

   --  Maximum zone IDs in a list result
   --  STM32H7S78 profile: 200 zones
   Max_Zone_Ids : constant := 200;

   --  =======================================================================
   --  Runtime Configuration
   --  =======================================================================

   --  Enable assertions and contracts (has plenty of memory)
   Enable_Contracts : constant Boolean := True;

   --  Enable debug output for development
   Enable_Debug : constant Boolean := True;

end Zoneinfo_Config;
