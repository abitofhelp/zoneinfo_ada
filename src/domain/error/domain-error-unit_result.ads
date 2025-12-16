pragma Ada_2022;
--  =========================================================================
--  Domain.Error.Unit_Result - Result monad for Unit (void) operations
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Pre-instantiated Result[Unit] for operations that return success/error
--    but no meaningful value. Used for side-effecting operations like I/O,
--    validation, etc.
--
--  Usage:
--    with Domain.Error.Unit_Result;
--
--    function Validate (Path : String) return Unit_Result.Result;
--    return Unit_Result.Ok (Domain.Unit.Unit_Value);
--
--  See Also:
--    Domain.Error.Result - Generic Result monad
--    Domain.Unit - The Unit type
--  =========================================================================

with Domain.Error.Result;
with Domain.Unit;

package Domain.Error.Unit_Result is new Domain.Error.Result.Generic_Result
  (T => Domain.Unit.Unit)
  with Preelaborate;
