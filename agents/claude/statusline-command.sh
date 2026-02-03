#!/bin/bash

# Read JSON input from stdin
input=$(cat)

# Extract data from JSON
current_dir=$(echo "$input" | jq -r '.workspace.current_dir')
model_name=$(echo "$input" | jq -r '.model.display_name')

# Get shortened path (show last 3 components or full path if shorter)
short_path=$(echo "$current_dir" | awk -F'/' '{n=NF; if(n<=3) print $0; else print "..." "/" $(n-2) "/" $(n-1) "/" $n}')

# Check for git repository and get branch
if git -C "$current_dir" rev-parse --is-inside-work-tree >/dev/null 2>&1; then
    branch=$(git -C "$current_dir" branch --show-current 2>/dev/null || echo 'HEAD')
    # Use printf with escape codes for git info
    git_info=$(printf " \033[1;37m⎇\033[0m \033[1;35m%s\033[0m" "$branch")
else
    git_info=""
fi

# Format the status line with colors using printf to properly handle escape sequences
# Structure: [user] dir [git branch] | model
printf "\033[1;33m%s\033[0m %s \033[1;37m|\033[0m \033[1;32m%s\033[0m" \
    "$short_path" \
    "$git_info" \
    "$model_name"
