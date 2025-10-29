name: "Base PRP Template v2 - Context-Rich with Validation Loops"
description: |

## Purpose

Template optimized for AI agents to implement features with sufficient context and self-validation capabilities to achieve working code through iterative refinement.

## Core Principles

1. **Context is King**: Include ALL necessary documentation, examples, and caveats
2. **Validation Loops**: Provide executable tests/lints the AI can run and fix
3. **Information Dense**: Use keywords and patterns from the codebase
4. **Progressive Success**: Start simple, validate, then enhance

---

## Goal

[What needs to be built - be specific about the end state and desires]

## Why

- [Business value and user impact]
- [Integration with existing features]
- [Problems this solves and for whom]

## What

[User-visible behavior and technical requirements]

### Success Criteria

- [ ] [Specific measurable outcomes]

## All Needed Context

### Documentation & References (list all context needed to implement the feature)

```yaml
# MUST READ - Include these in your context window
- url: [Official API docs URL]
  why: [Specific sections/methods you'll need]

- file: [path/to/example.py]
  why: [Pattern to follow, gotchas to avoid]

- doc: [Library documentation URL]
  section: [Specific section about common pitfalls]
  critical: [Key insight that prevents common errors]

- docfile: [PRPs/ai_docs/file.md]
  why: [docs that the user has pasted in to the project]
```

### Current relevant codebase tree (run `tree` in the root of the project) to get an overview of the codebase

```bash

```

### Desired relevant codebase tree with files to be added and responsibility of file

```bash

```

### Known Gotchas of our codebase & Library Quirks

```elixir
# CRITICAL: [Library name] requires [specific setup]
# Example: FastAPI requires async functions for endpoints
# Example: This ORM doesn't support batch inserts over 1000 records
# Example: We use Mock for tests and
```

## Implementation Blueprint

### Data models and structure

Create the core data models, we ensure type safety and consistency.

```elixir
Examples:
 - ecto schemas
 - internal context api functions
```

### List of tasks to be completed to fulfill the PRP in the order they should be completed

```yaml
Task 1:
MODIFY lib/existing_module.ex:
  - FIND pattern: "defmodule OldImplementation do"
  - INJECT after line containing "def function_name do"
  - PRESERVE existing method signatures

CREATE lib/new_feature.ex:
  - MIRROR pattern from: lib/similar_feature.ex
  - MODIFY module name and core logic
  - KEEP error handling pattern identical

...(...)

Task N:
...

```

### Per task pseudocode as needed added to each task

```elixir

# Task 1
# Pseudocode with CRITICAL details don't write entire code
@doc """
Transfers ownership of a team to a new user.

This function handles the complex process of changing team ownership while
maintaining data integrity and proper authorization checks.

## Critical Requirements:
- MUST validate that new_owner is a member of the team
- MUST verify current user has ownership permissions
- MUST update team.creator_id atomically
- MUST handle role transitions (old owner -> admin, new owner -> owner)
- MUST log ownership change for audit purposes

## Implementation Flexibility:
- MAY notify other team members of ownership change
- MAY validate organization-level permissions
- MAY implement additional business rules around ownership limits
"""
@spec transfer_team_ownership(Team.t(), User.t(), AuthorizationRequest.t()) ::
Authorization.result({:ok, Team.t()} | {:error, Changeset.t(Team.t()) | :invalid_transfer})
def transfer_team_ownership(%Team{} = team, %User{} = new_owner, %AuthorizationRequest{} = auth_request) do
  # PSEUDOCODE - Implementation details flexible but requirements mandatory:

  # 1. REQUIRED: Authorization check - current user must own the team
  with_authorization %{auth_request | context: :teams, check: :own_team, object: team} do

    # 2. REQUIRED: Validate new owner is team member
    case get_user_role_in_team(new_owner, team) do
      nil -> {:error, :user_not_team_member}
      _role ->
        # 3. REQUIRED: Database transaction for atomicity
        Repo.transaction(fn ->
        # 4. REQUIRED: Update team ownership
        # team |> change_team_owner(new_owner) |> Repo.update!()

        # 5. REQUIRED: Handle role transitions
        # - demote current owner to admin
        # - promote new owner to owner role

        # 6. REQUIRED: Create audit log entry
        # AuditLog.log_team_ownership_change(...)

        # 7. OPTIONAL: Additional business logic
        # - send notifications
        # - validate org limits
        # - custom validations
        end)
    end
  end
end
```

## Validation Loop

### Level 1: Syntax & Style

```bash
# Run these FIRST - fix any warnings and errors before proceeding
mix format
mix compile
# Expected: No errors. If errors, READ the error and fix.
```

### Level 2: Unit Tests each new feature/file/function use existing test patterns

```elixir
# CREATE test_new_feature.exs with extensive test cases
# USE existing patterns in filename generation and test generation
```

```bash
# Run and iterate until passing:
mix test
# If failing: Read error, understand root cause, fix code, re-run (never mock to pass)
```

### Level 3: Integration Test

Use the `tidewave` MCP server to test integrations and exercise the new, already-tested feature

### Level 4: Deployment & Creative Validation

```bash
# MCP servers or other creative validation methods
# Examples:
# - Load testing with realistic data
# - End-to-end user journey testing
# - Performance benchmarking
# - Security scanning
# - Documentation validation

# Custom validation specific to the feature
# [Add creative validation methods here]
```

## Final validation Checklist

- [ ] Code formatted: `mix format`
- [ ] All tests pass: `mix test`
- [ ] Manual test successful: [specific curl/command]
- [ ] Error cases handled gracefully
- [ ] Logs are informative but not verbose
- [ ] Documentation updated if needed

---

## Anti-Patterns to Avoid

- ❌ Don't create new patterns when existing ones work
- ❌ Don't skip validation because "it should work"
- ❌ Don't ignore failing tests - fix them
- ❌ Don't ignore new compilation warnings - fix them
- ❌ Don't use sync functions in async context
- ❌ Don't hardcode values that should be config
- ❌ Don't catch all exceptions - be specific
