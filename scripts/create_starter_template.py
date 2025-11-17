#!/usr/bin/env python3
"""
Create StarterLib template from StarterLib library.

This script:
1. Removes StarterLib-specific domain/application source code
2. Keeps hexagonal architecture infrastructure
3. Renames StarterLib → StarterLib throughout
4. Retains test framework but removes specific tests
5. Creates minimal starter examples
6. Updates all documentation
"""

import os
import re
import shutil
from pathlib import Path
from typing import List, Set

class StarterLibCreator:
    def __init__(self, project_root: str):
        self.project_root = Path(project_root)
        self.old_name_lower = "starterlib"
        self.old_name_pascal = "StarterLib"
        self.old_name_upper = "STARTERLIB"
        self.new_name_lower = "starterlib"
        self.new_name_pascal = "StarterLib"
        self.new_name_upper = "STARTERLIB"

    def run(self):
        """Execute all transformation steps."""
        print("=" * 70)
        print("CREATING STARTERLIB TEMPLATE FROM STARTERLIB")
        print("=" * 70)

        # Step 1: Remove StarterLib-specific source files
        print("\n🗑️  Step 1: Removing StarterLib-specific source files...")
        self.remove_domain_specific_files()

        # Step 2: Rename files
        print("\n📝 Step 2: Renaming files...")
        self.rename_files()

        # Step 3: Update file contents
        print("\n📝 Step 3: Updating file contents...")
        self.update_file_contents()

        # Step 4: Clean up test files
        print("\n🧹 Step 4: Cleaning up tests...")
        self.clean_test_files()

        # Step 5: Update documentation
        print("\n📚 Step 5: Updating documentation...")
        self.update_documentation()

        # Step 6: Create minimal starter examples
        print("\n📝 Step 6: Creating starter examples...")
        self.create_starter_examples()

        print("\n" + "=" * 70)
        print("✅ STARTERLIB TEMPLATE CREATED SUCCESSFULLY!")
        print("=" * 70)

    def remove_domain_specific_files(self):
        """Remove StarterLib-specific domain and application files."""
        # Directories to completely remove
        dirs_to_remove = [
            "src/domain/entity",
            "src/domain/service",
            "src/domain/value_object",
            "src/application/command",
            "src/application/model",
            "src/application/port",
            "src/application/usecase",
            "src/infrastructure/adapter",
            "src/infrastructure/cache",
            "src/infrastructure/paths",
            "src/infrastructure/platform",
            "src/api",
            "examples",
            "test/unit",
            "test/integration",
            "test/support",
        ]

        for dir_path in dirs_to_remove:
            full_path = self.project_root / dir_path
            if full_path.exists():
                shutil.rmtree(full_path)
                print(f"  ✓ Removed {dir_path}")

        # Create minimal directory structure
        dirs_to_create = [
            "src/domain/entity",
            "src/domain/service",
            "src/domain/value_object",
            "src/application/port/inbound",
            "src/application/port/outbound",
            "src/application/usecase",
            "src/infrastructure/adapter",
            "src/api",
            "examples",
            "test/unit",
            "test/integration",
        ]

        for dir_path in dirs_to_create:
            full_path = self.project_root / dir_path
            full_path.mkdir(parents=True, exist_ok=True)
            print(f"  ✓ Created {dir_path}")

    def rename_files(self):
        """Rename all files from starterlib to starterlib."""
        patterns = [
            ("*.gpr", self.rename_gpr_files),
            ("*.ads", self.rename_ada_files),
            ("*.adb", self.rename_ada_files),
        ]

        for pattern, rename_func in patterns:
            files = list(self.project_root.rglob(pattern))
            rename_func(files)

    def rename_gpr_files(self, files: List[Path]):
        """Rename GPR files."""
        for file_path in files:
            if self.old_name_lower in file_path.name:
                new_name = file_path.name.replace(self.old_name_lower, self.new_name_lower)
                new_path = file_path.parent / new_name
                file_path.rename(new_path)
                print(f"  ✓ Renamed {file_path.name} → {new_name}")

    def rename_ada_files(self, files: List[Path]):
        """Rename Ada source files."""
        for file_path in files:
            if self.old_name_lower in file_path.name:
                new_name = file_path.name.replace(self.old_name_lower, self.new_name_lower)
                new_path = file_path.parent / new_name
                file_path.rename(new_path)
                print(f"  ✓ Renamed {file_path.name} → {new_name}")

    def update_file_contents(self):
        """Update contents of all text files."""
        patterns = [
            "**/*.ads",
            "**/*.adb",
            "**/*.gpr",
            "**/*.toml",
            "**/*.md",
            "**/*.py",
            "**/*.sh",
            "**/Makefile",
        ]

        files_to_update: Set[Path] = set()
        for pattern in patterns:
            files_to_update.update(self.project_root.rglob(pattern))

        for file_path in files_to_update:
            if self.should_skip_file(file_path):
                continue

            try:
                with open(file_path, 'r', encoding='utf-8') as f:
                    content = f.read()

                # Replace all variations
                old_content = content
                content = content.replace(self.old_name_upper, self.new_name_upper)
                content = content.replace(self.old_name_pascal, self.new_name_pascal)
                content = content.replace(self.old_name_lower, self.new_name_lower)

                # Special replacements for common patterns
                content = content.replace("StarterLib_", "StarterLib_")
                content = content.replace("starterlib_", "starterlib_")

                if content != old_content:
                    with open(file_path, 'w', encoding='utf-8') as f:
                        f.write(content)
                    rel_path = file_path.relative_to(self.project_root)
                    print(f"  ✓ Updated {rel_path}")

            except Exception as e:
                print(f"  ⚠️  Error updating {file_path}: {e}")

    def should_skip_file(self, file_path: Path) -> bool:
        """Check if file should be skipped."""
        skip_patterns = [
            ".git/",
            "alire/",
            "__pycache__/",
            ".pyc",
            "obj/",
            "lib/",
            "coverage/",
        ]

        path_str = str(file_path)
        return any(pattern in path_str for pattern in skip_patterns)

    def clean_test_files(self):
        """Clean up test files, keep framework."""
        # Keep test/common (framework)
        # Remove specific test implementations
        test_dirs = [
            self.project_root / "test" / "unit",
            self.project_root / "test" / "integration",
        ]

        for test_dir in test_dirs:
            if test_dir.exists():
                # Remove all test files
                for test_file in test_dir.glob("test_*.ad[sb]"):
                    test_file.unlink()
                    print(f"  ✓ Removed {test_file.name}")

                # Create placeholder test runners
                if "unit" in str(test_dir):
                    self.create_placeholder_test_runner(test_dir, "unit")
                else:
                    self.create_placeholder_test_runner(test_dir, "integration")

    def create_placeholder_test_runner(self, test_dir: Path, test_type: str):
        """Create minimal test runner."""
        runner_content = f"""pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;

procedure {test_type.capitalize()}_Runner is
begin
   Put_Line ("========================================================");
   Put_Line ("  StarterLib {test_type.upper()} TEST SUITE");
   Put_Line ("========================================================");
   Put_Line ("");
   Put_Line ("Add your {test_type} tests here.");
   Put_Line ("");
   Put_Line ("========================================================");
   Put_Line ("  [PASS] Test suite ready for your tests");
   Put_Line ("========================================================");
end {test_type.capitalize()}_Runner;
"""

        runner_file = test_dir / f"{test_type}_runner.adb"
        with open(runner_file, 'w') as f:
            f.write(runner_content)
        print(f"  ✓ Created {runner_file.name}")

    def create_starter_examples(self):
        """Create minimal starter example."""
        example_dir = self.project_root / "examples"
        example_dir.mkdir(exist_ok=True)

        example_content = """pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;
with StarterLib;

procedure Example_1_Hello is
begin
   Put_Line ("Hello from StarterLib!");
   Put_Line ("This is a starter template for Ada 2022 libraries.");
   Put_Line ("Version: " & StarterLib.Version);
end Example_1_Hello;
"""

        example_file = example_dir / "example_1_hello.adb"
        with open(example_file, 'w') as f:
            f.write(example_content)
        print(f"  ✓ Created {example_file.name}")

    def update_documentation(self):
        """Update README and other docs."""
        readme_content = """# StarterLib - Ada 2022 Library Template

**Version**: 0.1.0
**Date**: November 13, 2025
**SPDX-License-Identifier**: BSD-3-Clause
**License File**: See the LICENSE file in the project root.
**Copyright**: © 2025 Michael Gardner, A Bit of Help, Inc.
**Status**: Template

---

[![License](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE)
[![Ada](https://img.shields.io/badge/Ada-2022-blue.svg)](https://ada-lang.io)
[![Alire](https://img.shields.io/badge/Alire-2.0-blue.svg)](https://alire.ada.dev)

Production-ready Ada 2022 library template with hexagonal architecture and functional error handling.

## Features

- ✅ **Hexagonal Architecture**: Clean separation (Domain, Application, Infrastructure)
- ✅ **Railway-Oriented Programming**: Functional error handling with Result monads
- ✅ **API Facade Pattern**: Stable public interface
- ✅ **Thread-Safe**: Protected concurrent operations
- ✅ **Cross-Platform**: Linux, macOS, BSD, Windows
- ✅ **Test Framework**: Ready for unit and integration tests
- ✅ **Build Profiles**: Standard, embedded, baremetal, concurrent
- ✅ **CI/CD Ready**: GitHub Actions workflow included
- ✅ **Release Automation**: Scripts for validation and publishing

## Quick Start

### Installation

```bash
# Using Alire
alr get starterlib
cd starterlib_*
alr build
```

### Basic Usage

```ada
with StarterLib;
with Ada.Text_IO; use Ada.Text_IO;

procedure My_App is
begin
   Put_Line ("Using StarterLib version: " & StarterLib.Version);
end My_App;
```

## Architecture

StarterLib uses **Hexagonal Architecture** (Ports and Adapters):

```
    ┌─────────────────────────────┐
    │   StarterLib.API (PUBLIC)   │  ← Users access here
    │   Facade Pattern            │
    └──────────┬──────────────────┘
               ↓
┌─────────────────────────────────┐
│  Application Layer (PRIVATE)    │
│    (Use Cases, Ports)           │
│                                  │
│  ┌───────────────────────────┐  │
│  │ Domain Layer (PUBLIC VOs) │  │  ← Value Objects
│  │  (Business Logic)         │  │     visible via API
│  └───────────────────────────┘  │
└─────────────────────────────────┘
         ↑              ↑
         │              │
  ┌──────┴──────┐  ┌───┴────────┐
  │Infrastructure│  │Infrastructure│
  │  (Adapters)  │  │  (Services)  │
  │  (PRIVATE)   │  │  (PRIVATE)  │
  └──────────────┘  └─────────────┘
```

**Key Design**: Only `StarterLib.API` and domain value objects are visible to users.
All other layers (Application, Infrastructure) are private implementation details.

## Requirements

- **Compiler**: GNAT FSF 14.2+ or GNAT Pro 25.0+
- **Ada Version**: Ada 2022
- **Build System**: Alire 2.0+
- **Dependencies**: functional ^2.0.0

## Development

```bash
# Build
make build

# Run tests
make test-all

# Format code
make format

# Generate coverage
make test-coverage
```

## License

Copyright © 2025 Michael Gardner, A Bit of Help, Inc.

Licensed under the BSD-3-Clause License. See [LICENSE](LICENSE) for details.

## Using This Template

1. Clone this repository
2. Run global find/replace: `StarterLib` → `YourLib`
3. Update `alire.toml` with your project details
4. Implement your domain logic in `src/domain/`
5. Add use cases in `src/application/`
6. Implement adapters in `src/infrastructure/`
7. Expose via `src/api/yourlib-api.ads`

## Support

- 📧 Email: support@abitofhelp.com
- 🐛 Issues: GitHub Issues
- 📖 Docs: See `docs/` directory
"""

        readme_file = self.project_root / "README.md"
        with open(readme_file, 'w') as f:
            f.write(readme_content)
        print(f"  ✓ Updated README.md")


def main():
    import sys

    if len(sys.argv) > 1:
        project_root = sys.argv[1]
    else:
        project_root = os.getcwd()

    creator = StarterLibCreator(project_root)
    creator.run()


if __name__ == "__main__":
    main()
