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
typeset -A ENVSCOPE_FILE_VARS        # Track which variables were set by which file
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
  
  while true; do
    printf "[envscope] Approve this .envrc file? (y/n/v=view): "
    read -k1 response
    echo  # newline after single char input
    case "$response" in
      [yY])
        ENVSCOPE_APPROVED_HASHES[$file]=$current_hash
        _envscope_save_approved_hashes
        echo "[envscope] Approved!"
        return 0
        ;;
      [nN])
        echo "[envscope] Not approved."
        return 1
        ;;
      [vV])
        echo "[envscope] File contents:"
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

# Show environment changes for a specific file by analyzing exports
_envscope_show_file_changes() {
  local file="$1"
  local -A old_state
  local var val
  
  # Copy old state
  for var in ${(k)ENVSCOPE_CURRENT_STATE}; do
    old_state[$var]="${ENVSCOPE_CURRENT_STATE[$var]}"
  done
  
  # Parse the .envrc file to find exported variables
  local changes=()
  local exported_vars=()
  
  # Extract variable names that are exported in this file
  while IFS= read -r line; do
    # Match export statements
    if [[ "$line" =~ ^[[:space:]]*export[[:space:]]+([A-Za-z0-9_]+)= ]]; then
      exported_vars+=(${match[1]})
    fi
  done < "$file"
  
  # Clear previous tracking for this file to avoid duplicates
  ENVSCOPE_FILE_VARS[$file]=""
  
  # Check each exported variable to see if it was new or modified
  for var in "${exported_vars[@]}"; do
    local current_value="${(P)var}"
    if [[ -z "${old_state[$var]}" ]]; then
      # New variable
      changes+=("+$var")
    elif [[ "${old_state[$var]}" != "$current_value" ]]; then
      # Modified variable  
      changes+=("~$var")
    fi
    # Track that this file sets this variable (no duplicates due to clear above)
    ENVSCOPE_FILE_VARS[$file]+="$var "
  done
  
  # Show changes if any
  if [[ ${#changes[@]} -gt 0 ]]; then
    echo "[envscope] $file | ${(j: :)changes}"
  fi
  
  # Update current state
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
    
    # Store original value only if we don't already have it AND it's not currently managed by envscope
    if [[ -z "${ENVSCOPE_ORIGINAL_VALUES[$var]}" && -z "${ENVSCOPE_SET_VARS[$var]}" ]]; then
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
    if [[ -z "${ENVSCOPE_ORIGINAL_VALUES[$var]}" ]]; then
      # Variable didn't exist originally
      ENVSCOPE_ORIGINAL_VALUES[$var]="__ENVSCOPE_UNSET__"
      ENVSCOPE_SET_VARS[$var]=1
    elif [[ "${ENVSCOPE_ORIGINAL_VALUES[$var]}" != "$val" ]]; then
      # Variable was changed from original
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
        # Capture state before this file
        _envscope_capture_current_state
        
        # Source the file (suppress echo output)
        source "$file" >/dev/null 2>/dev/null || echo "[envscope] Warning: Error sourcing $file"
        
        # Show changes for this specific file
        _envscope_show_file_changes "$file"
        
        ENVSCOPE_ACTIVE_ENVS[$file]=1
        loaded_any=1
      elif _envscope_request_approval "$file"; then
        # Capture state before this file
        _envscope_capture_current_state
        
        # Source the file (suppress echo output)  
        source "$file" >/dev/null 2>/dev/null || echo "[envscope] Warning: Error sourcing $file"
        
        # Show changes for this specific file
        _envscope_show_file_changes "$file"
        
        ENVSCOPE_ACTIVE_ENVS[$file]=1
        loaded_any=1
      else
        echo "[envscope] Skipping $file (not approved)"
      fi
    fi
  done
  
  if [[ $loaded_any -eq 1 ]]; then
    # Track what variables were set by all the .envrc files
    _envscope_track_changes
  fi
}

# Restore environment variables and show which variables are affected
_envscope_restore_environment() {
  local var
  local affected_vars=()
  
  for var in ${(k)ENVSCOPE_SET_VARS}; do
    affected_vars+=("$var")
    
    if [[ -n "${ENVSCOPE_ORIGINAL_VALUES[$var]}" ]]; then
      # Restore original value
      export "$var"="${ENVSCOPE_ORIGINAL_VALUES[$var]}"
    else
      # Variable was newly created, unset it
      unset "$var"
    fi
    unset "ENVSCOPE_SET_VARS[$var]"
    unset "ENVSCOPE_ORIGINAL_VALUES[$var]"
  done
  
  # Return the list of affected variables
  echo "${affected_vars[@]}"
}

# Unload .envrc files that are no longer in scope
_envscope_unload_envrc() {
  local current_files=($(_envscope_find_envrc_files))
  local active_file
  local should_unload
  local unloaded_any=0
  local unloaded_files=()
  
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
      # Get variables set by this specific file
      local file_vars=(${=ENVSCOPE_FILE_VARS[$active_file]})
      
      if [[ ${#file_vars[@]} -gt 0 ]]; then
        local unload_changes=()
        
        # Determine what happens to each variable during unloading
        for var in "${file_vars[@]}"; do
          # Check if this variable is used by any remaining .envrc files
          local var_used_elsewhere=0
          local new_value=""
          
          for remaining_file in "${current_files[@]}"; do
            if [[ "$ENVSCOPE_FILE_VARS[$remaining_file]" == *"$var "* ]]; then
              var_used_elsewhere=1
              # Get the value from the remaining file by parsing it
              while IFS= read -r line; do
                if [[ "$line" =~ ^[[:space:]]*export[[:space:]]+${var}=\"?([^\"]*) ]]; then
                  new_value="${match[1]}"
                  new_value="${new_value%\"}"  # Remove trailing quote if present
                  break
                fi
              done < "$remaining_file"
              break
            fi
          done
          
          # Determine the change type
          if [[ $var_used_elsewhere -eq 1 ]]; then
            # Variable will be changed to value from remaining file
            unload_changes+=("~$var")
          else
            # Check if variable will be restored to original or removed
            if [[ -n "${ENVSCOPE_ORIGINAL_VALUES[$var]}" ]]; then
              unload_changes+=("~$var")  # Restored to original
            else
              unload_changes+=("-$var")  # Removed entirely
            fi
          fi
          
          # Actually perform the unloading
          if [[ $var_used_elsewhere -eq 0 ]]; then
            local original_value="${ENVSCOPE_ORIGINAL_VALUES[$var]}"
            if [[ -n "$original_value" && "$original_value" != "__ENVSCOPE_UNSET__" ]]; then
              export "$var"="$original_value"
            else
              unset "$var"
            fi
            unset "ENVSCOPE_SET_VARS[$var]"
            unset "ENVSCOPE_ORIGINAL_VALUES[$var]"
          fi
        done
        
        echo "[envscope] Unloading $active_file | ${(j: :)unload_changes}"
      fi
      
      unset "ENVSCOPE_FILE_VARS[$active_file]"
      unset "ENVSCOPE_ACTIVE_ENVS[$active_file]"
      unloaded_any=1
    fi
  done
  
  if [[ $unloaded_any -eq 1 ]]; then
    # Reload remaining .envrc files in current scope
    local remaining_files=($(_envscope_find_envrc_files))
    if [[ ${#remaining_files[@]} -gt 0 ]]; then
      _envscope_load_envrc
    else
      # No remaining files - clean up any leftover variables
      for var in ${(k)ENVSCOPE_SET_VARS}; do
        local original_value="${ENVSCOPE_ORIGINAL_VALUES[$var]}"
        if [[ -n "$original_value" && "$original_value" != "__ENVSCOPE_UNSET__" ]]; then
          export "$var"="$original_value"
        else
          unset "$var"
        fi
        unset "ENVSCOPE_SET_VARS[$var]"
        unset "ENVSCOPE_ORIGINAL_VALUES[$var]"
      done
      # Clear all file variable tracking
      ENVSCOPE_FILE_VARS=()
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