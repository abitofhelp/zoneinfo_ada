pragma Ada_2022;
--  ===========================================================================
--  Zoneinfo - Timezone Library Root Package
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Root package for the Zoneinfo library. Provides high-level timezone-aware
--    datetime and duration operations for Ada 2022. Built on TZif for IANA
--    timezone database access.
--
--  Quick Start:
--    with Zoneinfo.API;
--
--    procedure Main is
--       use Zoneinfo.API;
--       Now_Result : constant Instant_Result.Result := Now;
--    begin
--       if Instant_Result.Is_Ok (Now_Result) then
--          --  Use Instant_Result.Value (Now_Result)
--       end if;
--    end Main;
--
--  Architecture:
--    This library follows hybrid DDD/Clean/Hexagonal architecture:
--    - Domain: Pure business logic (Instant, Civil, Zoned, Duration)
--    - Application: Use cases and ports (timezone operations)
--    - Infrastructure: Adapters (TZif integration)
--    - API: Public facade (Zoneinfo.API)
--
--  Public API:
--    See Zoneinfo.API for the main library interface
--
--  Embedded Safety:
--    This library is designed for use in embedded systems. All packages
--    use bounded types, static allocation, and avoid heap allocation.
--    The restrictions below enforce embedded-safe patterns.
--
--  ===========================================================================

--  ==========================================================================
--  Embedded Safety Restrictions
--  ==========================================================================
--  Per Ada Agent and SPARK Agent requirements, library projects MUST be
--  embedded-safe by default. These restrictions ensure the library can be
--  used in safety-critical and resource-constrained environments.
--
--  Note: Restrictions must be listed BEFORE the package declaration.
--  ==========================================================================

pragma Restrictions (No_Implicit_Heap_Allocations);
--  Prevents hidden heap allocations from language features

pragma Restrictions (No_Anonymous_Allocators);
--  Prevents anonymous access types from allocating

pragma Restrictions (No_Coextensions);
--  Prevents coextension allocations

package Zoneinfo with Pure is

   --  Library version information
   --  Note: Actual version is in Zoneinfo.Version package
   Lib_Version : constant String := "1.0.0";

end Zoneinfo;
