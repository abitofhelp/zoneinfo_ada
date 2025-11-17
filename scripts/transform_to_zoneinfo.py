#!/usr/bin/env python3
"""
Transform zoneinfo project to starterlib starter template.

Performs comprehensive renaming:
- zoneinfo → starterlib (lowercase, crate name)
- ZoneInfo → StarterLib (PascalCase, project/package names)
- ZONEINFO → STARTERLIB (uppercase, environment variables)
"""
import os
import re
from pathlib import Path
from typing import List, Tuple

# Project root
ROOT_DIR = Path(__file__).parent.parent

# Files and directories to exclude
EXCLUDE_DIRS = {'.git', 'alire', 'obj', 'lib', 'tools', 'coverage', '.claude'}
EXCLUDE_FILES = {'transform_to_starterlib.py'}

# File patterns to process
INCLUDE_PATTERNS = [
    '*.gpr',
    '*.ads',
    '*.adb',
    '*.md',
    'Makefile',
    '*.yaml',
    '*.yml',
    '*.toml',
]

def should_process_file(file_path: Path) -> bool:
    """Check if file should be processed."""
    # Exclude specific files
    if file_path.name in EXCLUDE_FILES:
        return False

    # Check if any parent directory is in exclude list
    for parent in file_path.parents:
        if parent.name in EXCLUDE_DIRS:
            return False

    # Check if matches any include pattern
    for pattern in INCLUDE_PATTERNS:
        if file_path.match(pattern):
            return True

    return False

def get_files_to_process(root: Path) -> List[Path]:
    """Get all files that should be processed."""
    files = []
    for file_path in root.rglob('*'):
        if file_path.is_file() and should_process_file(file_path):
            files.append(file_path)
    return files

def transform_content(content: str) -> Tuple[str, int]:
    """
    Transform content with all replacements.
    Returns (transformed_content, number_of_changes).
    """
    original = content
    changes = 0

    # Replacement patterns (order matters - most specific first)
    replacements = [
        # Package/Project names (PascalCase)
        (r'\bZoneInfo_Config\b', 'StarterLib_Config'),
        (r'\bZoneInfo\b', 'StarterLib'),

        # Lowercase package names (in with clauses, file references)
        (r'\bzoneinfo_config\b', 'starterlib_config'),
        (r'\bzoneinfo\b', 'starterlib'),

        # Environment variables (UPPERCASE)
        (r'\bZONEINFO_BUILD_MODE\b', 'STARTERLIB_BUILD_MODE'),
        (r'\bZONEINFO_PROFILE\b', 'STARTERLIB_PROFILE'),
        (r'\bZONEINFO\b', 'STARTERLIB'),

        # File paths and references
        (r'/zoneinfo/', '/starterlib/'),
        (r'zoneinfo_ada', 'hybrid_lib_starter'),

        # Specific content replacements
        (r'IANA Timezone Information Library', 'Library Starter Template'),
        (r'High-level timezone calculations and transformations',
         'Production-ready library starter with hexagonal architecture'),
        (r'timezone', 'your domain'),  # Generic placeholder
    ]

    for pattern, replacement in replacements:
        new_content = re.sub(pattern, replacement, content)
        if new_content != content:
            changes += 1
            content = new_content

    return content, changes

def process_file(file_path: Path) -> Tuple[bool, int]:
    """
    Process a single file.
    Returns (was_modified, number_of_changes).
    """
    try:
        # Read file
        content = file_path.read_text(encoding='utf-8')

        # Transform content
        new_content, changes = transform_content(content)

        # Write back if changed
        if changes > 0:
            file_path.write_text(new_content, encoding='utf-8')
            return True, changes

        return False, 0

    except Exception as e:
        print(f"  ❌ Error processing {file_path}: {e}")
        return False, 0

def rename_config_files(root: Path) -> int:
    """Rename zoneinfo_config.ads files to starterlib_config.ads."""
    count = 0
    for config_file in root.rglob('**/zoneinfo_config.ads'):
        if 'alire' in str(config_file) or 'obj' in str(config_file):
            continue

        new_name = config_file.parent / 'starterlib_config.ads'
        print(f"  Renaming: {config_file.relative_to(root)} → {new_name.name}")
        config_file.rename(new_name)
        count += 1

    return count

def main():
    """Main transformation process."""
    print("=" * 70)
    print("ZoneInfo → StarterLib Transformation")
    print("=" * 70)
    print()

    # Get files to process
    print("📂 Scanning for files...")
    files = get_files_to_process(ROOT_DIR)
    print(f"   Found {len(files)} files to process")
    print()

    # Rename config files first
    print("📝 Renaming configuration files...")
    renamed = rename_config_files(ROOT_DIR)
    print(f"   Renamed {renamed} config files")
    print()

    # Process each file
    print("🔄 Transforming file contents...")
    total_modified = 0
    total_changes = 0

    for file_path in sorted(files):
        modified, changes = process_file(file_path)
        if modified:
            rel_path = file_path.relative_to(ROOT_DIR)
            print(f"  ✓ {rel_path} ({changes} changes)")
            total_modified += 1
            total_changes += changes

    print()
    print("=" * 70)
    print(f"✅ Transformation complete!")
    print(f"   Modified: {total_modified} files")
    print(f"   Total changes: {total_changes}")
    print("=" * 70)
    print()
    print("Next steps:")
    print("  1. Review changes: git diff")
    print("  2. Test build: alr build")
    print("  3. Clean object files: make clean")
    print("  4. Rebuild: make build")
    print()

if __name__ == '__main__':
    main()
