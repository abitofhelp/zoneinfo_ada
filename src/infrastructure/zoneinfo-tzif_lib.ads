pragma Ada_2022;
--  =========================================================================
--  Zoneinfo.TZif_Lib - Library Renaming Alias for TZif
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Provides an alias for the external TZif library to avoid name shadowing.
--    The package Infrastructure.Adapter.Tzif shadows the library name "TZif",
--    so this renaming allows unambiguous access to the external library.
--
--  Usage:
--    with Zoneinfo.TZif_Lib.API.Desktop;  -- instead of: with TZif.API.Desktop
--
--  See Also:
--    Infrastructure.Adapter.Tzif - Uses this alias to access TZif library
--  =========================================================================

with TZif;

package Zoneinfo.TZif_Lib renames TZif;
