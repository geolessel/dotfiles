#!/usr/bin/env zsh
# -*- lexical-binding: t -*-
#
# envscope - A secure directory environment manager for zsh
# Automatically loads .envrc files with SHA-based security approval
# and provides hierarchical environment management with smart unloading.

# Namespace for envscope to avoid conflicts
typeset -A ENVSCOPE_APPROVED_HASHES
typeset -A ENVSCOPE_ACTIVE_ENVS
typeset -A ENVSCOPE_ORIGINAL_VALUES  # Store original values of modified variables
typeset -A ENVSCOPE_SET_VARS         # Track which variables were set by envscope
typeset -A ENVSCOPE_CURRENT_STATE
typeset ENVSCOPE_CONFIG_DIR="$HOME/.config/envscope"
typeset ENVSCOPE_APPROVED_FILE="$ENVSCOPE_CONFIG_DIR/approved_hashes"

# Initialize envscope
_envscope_init() {
  # Create config directory if it doesn't exist
  [[ ! -d "$ENVSCOPE_CONFIG_DIR" ]] && mkdir -p "$ENVSCOPE_CONFIG_DIR"
  
  # Load approved hashes from file
  if [[ -f "$ENVSCOPE_APPROVED_FILE" ]]; then
    while IFS='|' read -r hash file; do
      ENVSCOPE_APPROVED_HASHES[$file]=$hash
    done < "$ENVSCOPE_APPROVED_FILE"
  fi
  
  # Capture initial environment state
  _envscope_capture_current_state
}

# Capture current environment state
_envscope_capture_current_state() {
  ENVSCOPE_CURRENT_STATE=()
  local var val
  
  # Use a safer method to capture environment
  while IFS='=' read -r var val; do
    # Skip if no variable name
    [[ -z "$var" ]] && continue
    # Skip variables with problematic characters
    [[ "$var" =~ [^A-Za-z0-9_] ]] && continue
    
    ENVSCOPE_CURRENT_STATE[$var]="$val"
  done < <(env)
}

# Calculate SHA256 hash of a file
_envscope_hash_file() {
  local file="$1"
  if command -v sha256sum >/dev/null 2>&1; then
    sha256sum "$file" | cut -d' ' -f1
  elif command -v shasum >/dev/null 2>&1; then
    shasum -a 256 "$file" | cut -d' ' -f1
  else
    echo "ERROR: No SHA256 utility found (sha256sum or shasum)" >&2
    return 1
  fi
}

# Save approved hashes to file
_envscope_save_approved_hashes() {
  # Ensure directory exists
  if [[ ! -d "$ENVSCOPE_CONFIG_DIR" ]]; then
    mkdir -p "$ENVSCOPE_CONFIG_DIR" || {
      echo "[envscope] ERROR: Failed to create config directory" >&2
      return 1
    }
  fi
  
  local file hash content=""
  
  # Build content string first
  for file hash in ${(kv)ENVSCOPE_APPROVED_HASHES}; do
    content="${content}${hash}|${file}\n"
  done
  
  # Write content to file
  printf "$content" > "$ENVSCOPE_APPROVED_FILE" || {
    echo "[envscope] ERROR: Failed to write approved hashes file" >&2
    return 1
  }
}

# Check if .envrc file is approved
_envscope_is_approved() {
  local file="$1"
  local current_hash=$(_envscope_hash_file "$file")
  local approved_hash="${ENVSCOPE_APPROVED_HASHES[$file]}"
  
  [[ "$current_hash" == "$approved_hash" ]]
}

# Prompt user for approval of .envrc file
_envscope_request_approval() {
  local file="$1"
  local current_hash=$(_envscope_hash_file "$file")
  
  echo "[envscope] Found new or modified .envrc file: $file"
  echo "[envscope] Contents:"
  echo "----------------------------------------"
  cat "$file" | sed 's/^/  /'
  echo "----------------------------------------"
  
  while true; do
    printf "[envscope] Approve this .envrc file? (y/n/v=view): "
    read -k1 response
    echo  # newline after single char input
    case "$response" in
      [yY])
        echo "[envscope] Setting hash..."
        ENVSCOPE_APPROVED_HASHES[$file]=$current_hash
        echo "[envscope] Saving to file..."
        _envscope_save_approved_hashes
        echo "[envscope] Approved!"
        return 0
        ;;
      [nN])
        echo "[envscope] Not approved."
        return 1
        ;;
      [vV])
        echo "[envscope] Viewing file contents:"
        echo "----------------------------------------"
        cat "$file" | sed 's/^/  /'
        echo "----------------------------------------"
        ;;
      *)
        echo "[envscope] Please press y, n, or v"
        ;;
    esac
  done
}

# Find all .envrc files from home directory down to current directory
_envscope_find_envrc_files() {
  local current_dir="$PWD"
  local home_dir="$HOME"
  local files=()
  
  # Build array of all parent directories from home to current
  local dir="$current_dir"
  local all_dirs=()
  
  # Walk up from current directory, collecting all directories
  while [[ "$dir" != "/" && "$dir" != "$home_dir" ]]; do
    all_dirs=("$dir" "${all_dirs[@]}")
    dir="$(dirname "$dir")"
  done
  
  # Add home directory if we reached it
  if [[ "$dir" == "$home_dir" ]]; then
    all_dirs=("$home_dir" "${all_dirs[@]}")
  fi
  
  # If we never reached home (e.g., /tmp path), include the path from root
  if [[ "$dir" != "$home_dir" ]]; then
    # For paths outside home, just use current directory hierarchy
    dir="$current_dir"
    all_dirs=()
    while [[ "$dir" != "/" ]]; do
      all_dirs=("$dir" "${all_dirs[@]}")
      dir="$(dirname "$dir")"
    done
  fi
  
  # Add .envrc for each directory in the path
  for dir in "${all_dirs[@]}"; do
    files+=("$dir/.envrc")
  done
  
  # Filter to only existing files
  local existing_files=()
  for file in "${files[@]}"; do
    [[ -f "$file" ]] && existing_files+=("$file")
  done
  echo "${existing_files[@]}"
}

# Compare environment states and show changes
_envscope_show_env_changes() {
  # Simple approach - just show that environment was loaded
  # The complex diff was likely causing the hang
  echo "[envscope] Environment loaded successfully"
  
  # Update current state for next comparison
  _envscope_capture_current_state
}

# Capture environment state before loading .envrc
_envscope_capture_pre_load_state() {
  local var val
  while IFS='=' read -r var val; do
    [[ -z "$var" ]] && continue
    [[ "$var" =~ [^A-Za-z0-9_] ]] && continue
    
    # Skip system variables that shouldn't be restored
    case "$var" in
      PWD|OLDPWD|SHLVL|_) continue ;;
    esac
    
    # Store original value if we don't already have it
    if [[ -z "${ENVSCOPE_ORIGINAL_VALUES[$var]}" ]]; then
      ENVSCOPE_ORIGINAL_VALUES[$var]="$val"
    fi
  done < <(env)
}

# Track which variables were set/modified by .envrc loading  
_envscope_track_changes() {
  local var val
  while IFS='=' read -r var val; do
    [[ -z "$var" ]] && continue
    [[ "$var" =~ [^A-Za-z0-9_] ]] && continue
    
    # Skip system variables that shouldn't be restored
    case "$var" in
      PWD|OLDPWD|SHLVL|_) continue ;;
    esac
    
    # Check if this variable was set or changed
    if [[ -z "${ENVSCOPE_ORIGINAL_VALUES[$var]}" || "${ENVSCOPE_ORIGINAL_VALUES[$var]}" != "$val" ]]; then
      ENVSCOPE_SET_VARS[$var]=1
    fi
  done < <(env)
}

# Load .envrc files for current directory
_envscope_load_envrc() {
  local files=($(_envscope_find_envrc_files))
  local loaded_any=0
  
  # Capture environment before loading
  _envscope_capture_pre_load_state
  
  for file in "${files[@]}"; do
    if [[ -f "$file" ]]; then
      if _envscope_is_approved "$file"; then
        echo "[envscope] Loading $file (approved)"
        source "$file" 2>/dev/null || echo "[envscope] Warning: Error sourcing $file"
        ENVSCOPE_ACTIVE_ENVS[$file]=1
        loaded_any=1
      elif _envscope_request_approval "$file"; then
        echo "[envscope] Loading $file (newly approved)"
        source "$file" 2>/dev/null || echo "[envscope] Warning: Error sourcing $file"
        ENVSCOPE_ACTIVE_ENVS[$file]=1
        loaded_any=1
      else
        echo "[envscope] Skipping $file (not approved)"
      fi
    fi
  done
  
  if [[ $loaded_any -eq 1 ]]; then
    # Track what variables were set by the .envrc files
    _envscope_track_changes
    echo "[envscope] Environment loaded successfully"
  fi
}

# Restore environment variables to their original state
_envscope_restore_environment() {
  local var
  local restored_vars=()
  local unset_vars=()
  
  for var in ${(k)ENVSCOPE_SET_VARS}; do
    if [[ -n "${ENVSCOPE_ORIGINAL_VALUES[$var]}" ]]; then
      # Restore original value
      export "$var"="${ENVSCOPE_ORIGINAL_VALUES[$var]}"
      restored_vars+=("$var")
    else
      # Variable was newly created, unset it
      unset "$var"
      unset_vars+=("$var")
    fi
    unset "ENVSCOPE_SET_VARS[$var]"
    unset "ENVSCOPE_ORIGINAL_VALUES[$var]"
  done
  
  # Show summary of changes
  if [[ ${#restored_vars[@]} -gt 0 || ${#unset_vars[@]} -gt 0 ]]; then
    echo "[envscope] Environment restored: ${#restored_vars[@]} restored, ${#unset_vars[@]} unset"
  fi
}

# Unload .envrc files that are no longer in scope
_envscope_unload_envrc() {
  local current_files=($(_envscope_find_envrc_files))
  local active_file
  local should_unload
  local unloaded_any=0
  
  # Check each active environment
  for active_file in ${(k)ENVSCOPE_ACTIVE_ENVS}; do
    should_unload=1
    
    # Check if this file is still in current scope
    for current_file in "${current_files[@]}"; do
      if [[ "$active_file" == "$current_file" ]]; then
        should_unload=0
        break
      fi
    done
    
    # Unload if no longer in scope
    if [[ $should_unload -eq 1 ]]; then
      echo "[envscope] Unloading $active_file"
      unset "ENVSCOPE_ACTIVE_ENVS[$active_file]"
      unloaded_any=1
    fi
  done
  
  if [[ $unloaded_any -eq 1 ]]; then
    # Restore original environment state
    _envscope_restore_environment
    
    # Reload remaining .envrc files in current scope
    local remaining_files=($(_envscope_find_envrc_files))
    if [[ ${#remaining_files[@]} -gt 0 ]]; then
      echo "[envscope] Reloading remaining environments..."
      _envscope_load_envrc
    fi
  fi
}

# Main function called on directory change
_envscope_chpwd() {
  _envscope_unload_envrc
  _envscope_load_envrc
}

# Hook into zsh's chpwd
autoload -U add-zsh-hook
add-zsh-hook chpwd _envscope_chpwd

# Initialize envscope
_envscope_init

# Load .envrc for current directory on startup
_envscope_load_envrc