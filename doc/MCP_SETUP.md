# MCP Setup for Clojure Projects with Claude Code

This guide documents how to set up the **clojure-mcp** MCP server for use with Claude Code in Clojure projects.

## Overview

The setup connects Claude Code to your Clojure project via:
1. **nREPL server** - running in your project for code evaluation
2. **clojure-mcp** - MCP server providing Clojure-aware tools to Claude Code
3. **Claude Code** - configured to use the MCP server

```
Claude Code  <-->  clojure-mcp (MCP Server)  <-->  nREPL  <-->  Your Project
```

## Prerequisites

- [Clojure CLI](https://clojure.org/guides/install_clojure) installed
- Java JDK 17+
- [ripgrep](https://github.com/BurntSushi/ripgrep#installation) (recommended for better search performance)
- Claude Code CLI installed
- An `:nrepl` alias in your project's `deps.edn` that starts an nREPL server, e.g.:
  ```clojure
  :nrepl {:extra-deps {nrepl/nrepl {:mvn/version "1.3.1"}
                       cider/cider-nrepl {:mvn/version "0.55.7"}}
          :main-opts  ["-m" "nrepl.cmdline"
                       "--middleware" "[cider.nrepl/cider-middleware]"]}
  ```
  (clojure-mcp can also attach to an already-running nREPL if you omit `:start-nrepl-cmd`.)

## Step 1: Install clojure-mcp

Clone the clojure-mcp repository:

```bash
git clone https://github.com/bhauman/clojure-mcp.git <clojure-mcp-dir>
cd <clojure-mcp-dir>
```

## Step 2: Add MCP Server to Claude Code

Use the `claude mcp add` command to register the server. This project builds with
**tools.deps** (`deps.edn` + `bb.edn`), so use the tools.deps nREPL command.

### For tools.deps Projects (recommended, with auto-start REPL)

```bash
claude mcp add clojure-mcp \
  /bin/bash \
  -s user \
  -- -c "cd <clojure-mcp-dir> && clojure -X:mcp :port 7901 :start-nrepl-cmd '[\"clojure\" \"-M:nrepl\"]' :project-dir '\"<project-dir>\"'"
```

`-M:nrepl` runs the `:nrepl` alias described in the Prerequisites.

### For Leiningen Projects (legacy)

If a project still uses Leiningen, the nREPL can be started the old way:

```bash
claude mcp add clojure-mcp \
  /bin/bash \
  -s user \
  -- -c "cd <clojure-mcp-dir> && clojure -X:mcp :port 7901 :start-nrepl-cmd '[\"lein\" \"repl\" \":headless\" \":port\" \"7901\"]' :project-dir '\"<project-dir>\"'"
```

### Without Auto-Start (manual REPL)

If you prefer to start the REPL manually:

```bash
claude mcp add clojure-mcp \
  /bin/bash \
  -s user \
  -- -c "cd <clojure-mcp-dir> && clojure -X:mcp :port 7901 :project-dir '\"<project-dir>\"'"
```

Then start your nREPL manually before using Claude Code:
```bash
clojure -M:nrepl   # tools.deps  (legacy: lein repl :headless :port 7901)
```

### Example for this project

`<project-dir>` is your local `desargues` checkout; `<clojure-mcp-dir>` is wherever you
cloned clojure-mcp:

```bash
claude mcp add clojure-mcp \
  /bin/bash \
  -s user \
  -- -c "cd <clojure-mcp-dir> && clojure -X:mcp :port 7901 :start-nrepl-cmd '[\"clojure\" \"-M:nrepl\"]' :project-dir '\"<project-dir>\"'"
```

## Step 3: Verify the Setup

Check the MCP server is configured:

```bash
claude mcp list
```

Get details about the server:

```bash
claude mcp get clojure-mcp
```

## Step 4: Configure Project Permissions (Optional but Recommended)

Create `.claude/settings.local.json` in your project to auto-allow MCP tools:

```json
{
  "permissions": {
    "allow": [
      "mcp__clojure-mcp__clojure_inspect_project",
      "mcp__clojure-mcp__read_file",
      "mcp__clojure-mcp__glob_files",
      "mcp__clojure-mcp__file_write",
      "mcp__clojure-mcp__clojure_edit_replace_sexp",
      "mcp__clojure-mcp__clojure_edit",
      "mcp__clojure-mcp__think",
      "mcp__clojure-mcp__bash",
      "mcp__clojure-mcp__file_edit",
      "mcp__clojure-mcp__scratch_pad",
      "mcp__clojure-mcp__clojure_eval",
      "mcp__clojure-mcp__LS"
    ]
  }
}
```

## Step 5: Add Claude Hooks (Optional)

Add hooks to `~/.claude/settings.json` for automatic Clojure formatting and paren repair:

```json
{
  "hooks": {
    "PreToolUse": [
      {
        "matcher": "Write|Edit",
        "hooks": [
          {
            "type": "command",
            "command": "clj-paren-repair-claude-hook --cljfmt"
          }
        ]
      }
    ],
    "PostToolUse": [
      {
        "matcher": "Edit|Write",
        "hooks": [
          {
            "type": "command",
            "command": "clj-paren-repair-claude-hook --cljfmt"
          }
        ]
      }
    ]
  }
}
```

Note: The `clj-paren-repair-claude-hook` command must be installed separately.

## MCP Command Reference

| Command | Description |
|---------|-------------|
| `claude mcp list` | List all configured MCP servers |
| `claude mcp get <name>` | Get details about a specific server |
| `claude mcp add <name> <cmd> [args...]` | Add a new MCP server |
| `claude mcp remove <name>` | Remove an MCP server |

### Add Command Options

- `-s user` - Add to user config (available in all projects)
- `-s project` - Add to project config (only this project)
- `--env KEY=VALUE` - Set environment variables

## Usage Workflow

1. **Start Claude Code** in your project directory:
   ```bash
   cd <project-dir>
   claude
   ```

2. The MCP server will automatically:
   - Start the nREPL (if `:start-nrepl-cmd` is configured)
   - Connect to your project

3. Verify connection with `/mcp` command in Claude Code

## Available MCP Tools

Once configured, Claude Code gains access to these Clojure-specific tools:

| Tool | Description |
|------|-------------|
| `clojure_eval` | Evaluate Clojure code in your REPL |
| `clojure_edit` | Edit top-level forms (defn, def, ns, etc.) |
| `clojure_edit_replace_sexp` | Replace s-expressions within forms |
| `clojure_inspect_project` | Analyze project structure and dependencies |
| `read_file` | Smart file reader with collapsed view for Clojure files |
| `glob_files` | Find files by pattern |
| `grep` | Search file contents |
| `file_edit` | Edit files with linting for Clojure |
| `file_write` | Write files with formatting for Clojure |
| `bash` | Execute shell commands |
| `scratch_pad` | Persistent storage for planning and state |
| `dispatch_agent` | Launch sub-agents for complex searches |
| `architect` | Get implementation planning help |
| `code_critique` | Interactive code review |
| `think` | Log reasoning and brainstorming |
| `LS` | List directory contents |

## Switching Projects

To switch to a different project:

1. **Remove the existing server**:
   ```bash
   claude mcp remove clojure-mcp -s user
   ```

2. **Add with new project path**:
   ```bash
   claude mcp add clojure-mcp \
     /bin/bash \
     -s user \
     -- -c "cd <clojure-mcp-dir> && clojure -X:mcp :port 7901 :start-nrepl-cmd '[\"clojure\" \"-M:nrepl\"]' :project-dir '\"<new-project-dir>\"'"
   ```

Alternatively, configure multiple servers with different names for different projects.

## Troubleshooting

### Check MCP server status

```bash
claude mcp list
claude mcp get clojure-mcp
```

### MCP server not connecting

1. Verify nREPL port is available:
   ```bash
   lsof -i :7901
   ```

2. Check if clojure-mcp can start:
   ```bash
   cd <clojure-mcp-dir>
   clojure -X:mcp :port 7901 :project-dir '"<project-dir>"'
   ```

3. Check Claude Code MCP status:
   ```
   /mcp
   ```

### Evaluation errors

- Ensure your REPL has loaded the required namespaces
- Use `:reload` when requiring namespaces to get latest changes:
  ```clojure
  (require '[your.namespace :reload])
  ```

### File editing issues

- The MCP tools lint Clojure code before writing
- If edits fail, check for syntax errors in the proposed changes

### Remove and re-add

```bash
claude mcp remove clojure-mcp -s user
# Then add again with corrected parameters
```

## Configuration Files (Reference)

The `claude mcp add` command writes to `~/.claude/mcp.json`:

```json
{
  "mcpServers": {
    "clojure-mcp": {
      "command": "/bin/bash",
      "args": [
        "-c",
        "cd <clojure-mcp-dir> && clojure -X:mcp :port 7901 :start-nrepl-cmd '[\"clojure\" \"-M:nrepl\"]' :project-dir '\"<project-dir>\"'"
      ]
    }
  }
}
```

## Resources

- [clojure-mcp GitHub](https://github.com/bhauman/clojure-mcp)
- [clojure-mcp-light](https://github.com/bhauman/clojure-mcp-light) - Lighter alternative for Claude Code
- [Claude Code Documentation](https://code.claude.com/docs)
- [MCP Protocol](https://modelcontextprotocol.io/)
