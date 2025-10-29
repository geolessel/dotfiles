# Execute BASE PRP

Implement a feature using its PRP file without direct human intervention.

## PRP File: $ARGUMENTS

## Execution Process

1. **Load PRP**
   - Read the specified PRP file
   - Understand all context and requirements
   - Follow all instructions in the PRP and extend the research if needed
   - Ensure you have all needed context to implement the PRP fully
   - Do more web searches and codebase exploration as needed

2. **ULTRATHINK**
   - Ultrathink before you execute the plan. Create a comprehensive plan addressing all requirements.
   - Break down the PRP into clear todos using your todo tools.
   - Use agents, subagents, and batchtool to enhance the process.
   - **Important** YOU MUST ENSURE YOU HAVE EXTREMELY CLEAR TASKS FOR SUBAGENTS AND REFERENCE CONTEXT AND MAKE SURE EACH SUBAGENT READS THE PRP AND UNDERSTANDS ITS CONTEXT.
   - Identify implementation patterns from existing code to follow.
   - Never guess about imports, file names funtion names etc, ALWAYS be based in reality and real context gathering

3. **Execute the PRP step by step**
   - Implement the smallest set of code that doesn't break the existing build but also gets us closer to finishing the feature
   - A set of code should have accompanying tests if applicable

4. **Validate**
   - Run each validation command
   - The better validation that is done, the more confident we can be that the implementation is correct.
   - Fix any failures
   - Resolve any compilation warnings in our code
   - Re-run until all pass
   - Always re-read the PRP to validate and review the implementation to ensure it meets the requirements

5. **Commit**
   - Follow the "Incremental Development Philosophy" section below
   - Commit your work into git
   - Write a descriptive summary line and put relevant details in the commit message
   - Each commit should tell a story and stand on its own
   - IT IS IMPORTANT THAT BROKEN CODE NOT BE COMMITED INTO GIT. Make sure step 4 above is complete.
   - Err on the side of too many commits over too few commits. It is easier to later squash commits than split them apart.

6. **Complete**
   - Ensure all checklist items done
   - Run final validation suite
   - Report completion status
   - Read the PRP again to ensure you have implemented everything

7. **Reference the PRP**
   - You can always reference the PRP again if needed

## Incremental Development Philosophy

When working on any task that involves multiple changes, ALWAYS break it down into the smallest possible commits.
Each commit should:
- Represent a single, complete, working change
- Include tests for any new functionality
- Pass all existing tests
- Be immediately commitable without breaking the build

### Required Workflow for Refactoring/Migration Tasks

When migrating from one function/approach to another, follow this EXACT sequence:

1. **Create the new implementation** with comprehensive tests
2. **Commit the new implementation** (old code still exists and is used)
3. **Update callers** to use the new implementation (one file or logical group at a time)
4. **Commit each set of caller updates** separately
5. **Remove the old implementation** only after all callers are migrated
6. **Commit the removal** as a final step

### General Rules for All Tasks

- **Never make large, multi-purpose commits**
- **Always run tests before committing** and ensure they pass
- **Each commit message should describe a single logical change**
- **If a task seems too large for one commit, break it down further**
- **When in doubt, make smaller commits rather than larger ones**
- **Always commit working, tested code - never commit broken or untested code**

### Examples of Good Commit Sequences

**Refactoring Example:**
1. "Add new extract_content_from_file/2 function with tests"
2. "Update FileProcessor to use new extract_content_from_file/2"
3. "Update DocumentWorker to use new extract_content_from_file/2"
4. "Remove deprecated extract_file_content/2 function"

**Feature Example:**
1. "Add User schema field for email_notifications"
2. "Add UserNotifications context with tests"
3. "Add email notification UI components"
4. "Wire up notification settings in user profile"

### Commit Requirements

Before making ANY commit:
1. Run the test suite (`mix test`)
2. Run the linter (`mix format`, `npm run lint:fix`)
3. Ensure the application still builds and runs
4. Write a descriptive commit message explaining the single change

If any of these fail, fix the issues before committing.

The key principles this enforces:
- Single responsibility per commit
- Always working code (no broken intermediate states)
- Tests before implementation commits
- Explicit migration workflow for refactoring
- Build/test validation before each commit

This approach ensures that each commit is a safe, revertible unit of work that maintains project stability
throughout the development process.


Note: If validation fails, use error patterns in PRP to fix and retry.
