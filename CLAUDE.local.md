<!-- BEGIN code-rag managed block — do not edit between markers; bld rewrites it -->
This repository is indexed by Code-RAG, a local MCP server. Choose your
search tool by WHAT YOU KNOW, not by habit:

- You know the exact token (symbol, string, file name, import) →
  **Grep / Glob**. Nothing beats it when you can name the thing.
- You know the CONCEPT but not the token ("where do we enforce the
  session timeout", "what calculates benefit cost", "how is X wired
  up") → **`search_code`**. Guessing keywords for Grep is the failure
  mode it exists to remove.
- You want to know WHY something is the way it is, when it changed, or
  what moved with it → **`search_history`** (indexed git and Subversion
  commit messages). No amount of reading the tree answers this.

`search_code` returns ranked FILES by default, each with the symbols that
matched and an excerpt already widened to the whole enclosing function, so
one call is usually enough — Read the returned path and line range only
when you need more context around it. It covers every repository in this
project at once, which a directory-scoped Grep cannot.

If the answer might live in a DIFFERENT repository, use the `code_rag_all`
server, which searches every indexed project and tags each hit with the
project it came from.

After you create or edit files, call `reindex_path` on them if you intend
to search for them in this session — the background sweep runs only every
few minutes, so your own new code is otherwise invisible to `search_code`.
<!-- END code-rag managed block -->
