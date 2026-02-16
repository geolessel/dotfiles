---
name: meeting-todos
description: Extract action items from Granola meetings and create them as todos in Things inbox.
argument-hint: [today | this week | last week | last 30 days | meeting title]
disable-model-invocation: true
allowed-tools: mcp__granola__query_granola_meetings, mcp__granola__list_meetings, mcp__granola__get_meetings, mcp__things__add_todo, AskUserQuestion
---

# Meeting Todos

You are helping me (Geoffrey or Geo) extract action items from meeting notes in Granola and create them as todos in my Things inbox.

## Step 1: Find meetings

Parse `$ARGUMENTS` to determine which meetings to look up:

| Argument | Behavior |
|----------|----------|
| *(empty)* or `today` | `list_meetings` with `this_week`, filter to today's date only |
| `yesterday` | `list_meetings` with `this_week` (or `last_week` if today is Monday), filter to yesterday's date only |
| `this week` | `list_meetings` with `this_week` |
| `last week` | `list_meetings` with `last_week` |
| `last 30 days` | `list_meetings` with `last_30_days` |
| Anything else | Treat as a meeting title search -- `list_meetings` with `last_30_days`, filter results by title containing `$ARGUMENTS` (case-insensitive) |

Report what was found: how many meetings and their titles/dates.

If no meetings are found, inform me and suggest alternatives (e.g., "No meetings found for today. Try `/meeting-todos this week` or `/meeting-todos [meeting title]`"). Stop here.

## Step 2: Extract action items

For the meetings found in Step 1:

1. Call `query_granola_meetings` scoped to the found meeting IDs, asking: "What are the action items, todos, follow-ups, and next steps from these meetings?"
2. Call `get_meetings` for each meeting to cross-reference the AI-generated summaries and private notes for any additional action items.

From the combined results:

- **Include**: Action items assigned to me, and items where I need to follow up with someone else (e.g., "Follow up with Alex on API docs").
- **Exclude**: Items that are purely someone else's responsibility with no follow-up needed from me.
- **Deduplicate**: If the same action item appears in both the query results and the meeting summary, keep only one copy.

If no action items are found, inform me and stop.

## Step 3: Confirm with user

Present the extracted action items as a numbered list, grouped by meeting:

```
## Meeting Title (YYYY-MM-DD)
1. Action item one
2. Action item two

## Another Meeting (YYYY-MM-DD)
3. Action item three
```

Use `AskUserQuestion` to let me choose what to create. Options:

- **Create all** -- create todos for every item listed
- **Select specific items** -- let me specify which numbers to include (e.g., "1, 3, 5")
- **Cancel** -- create nothing

If I cancel, stop here. No todos are created.

## Step 4: Create in Things

For each confirmed action item, call `add_todo` with:

- **title**: Concise action item in imperative mood (e.g., "Review API docs with Alex", "Send updated timeline to stakeholders")
- **notes**: `From meeting: [Meeting Title] (YYYY-MM-DD)` -- include the Granola citation link if one is available (e.g., `[[0]](url)`)
- **tags**: `["meeting"]`
- **area**: "Planning Center"
- Do NOT set `when`, `deadline`, `list_id`, or `heading_id` -- todos go straight to the inbox for me to triage

After creating all todos, report what was created: how many todos, which meetings they came from.

## Guidelines

- Preserve all Granola citation links (e.g., `[[0]](url)`) in the notes field so I can trace back to the source
- Keep action item titles short and actionable -- imperative mood, no fluff
- If there is a general todo item that can't be traced to a specific person, include it
- When in doubt about whether an item is mine, include it -- I can always delete from the inbox
- Do not create duplicate todos if the same action item appears across multiple meetings

