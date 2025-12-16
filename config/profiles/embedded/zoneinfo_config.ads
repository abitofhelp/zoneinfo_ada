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

end Zoneinfo_Config;
