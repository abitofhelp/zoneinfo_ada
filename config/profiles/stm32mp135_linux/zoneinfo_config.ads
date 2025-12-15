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

   --  Maximum length of person names
   --  Linux MPU profile: 128 characters (server-class, maximum compatibility)
   Max_Name_Length : constant := 128;

   --  Maximum length of greeting messages
   --  Linux MPU profile: 256 characters
   Max_Message_Length : constant := 256;

   --  Maximum length of error messages
   --  Linux MPU profile: 512 characters
   Max_Error_Length : constant := 512;

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

   --  Maximum discovered timezone sources
   --  Linux MPU profile: 100 sources
   Max_Sources : constant := 100;

   --  Maximum zone IDs in a list result
   --  Linux MPU profile: 1000 zones
   Max_Zone_Ids : constant := 1_000;

   --  =======================================================================
   --  Runtime Configuration
   --  =======================================================================

   --  Enable assertions and contracts
   Enable_Contracts : constant Boolean := True;

   --  Enable debug output
   Enable_Debug : constant Boolean := True;

end Zoneinfo_Config;
