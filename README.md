# LLMV IR LSP

<!--toc:start-->
- [LLMV IR LSP](#llmv-ir-lsp)
  - [Installation](#installation)
  - [Editor setup](#editor-setup)
    - [Neovim](#neovim)
    - [VS Code](#vs-code)
<!--toc:end-->

This is an small [Language Server](https://microsoft.github.io/language-server-protocol/) to work with [LLVM IR in textual form](https://www.llvm.org/docs/LangRef.html) - the `.ll` files produced by compiler frontends.

It's not designed for editing, just supporting basic parsing and navigation, assuming read only mode. Should be useful for compiler 
engineers targeting LLVM IR in text form.

- [x] document symbols (metadata and functions)
- [x] workspace symbols (functions)
- [x] find references (functions from call instructions)
- [x] go to definition (functions)
- [x] hover

<img width="3316" height="2114" alt="CleanShot 2026-07-24 at 10 25 27@2x" src="https://github.com/user-attachments/assets/e56167ca-e425-495c-b3b0-c194696fa68a" />


## Installation

**Homebrew**:

```
brew install indoorvivants/tap/llvm-ir-lsp
```

**Manual**:

Download the binary for your platform from [Github releases](https://github.com/indoorvivants/llvm-ir-lsp/releases)

## Editor setup


### Neovim

Put this in your `init.lua` config:

```lua
vim.api.nvim_create_autocmd('FileType', {
  pattern = 'llvm',
  callback = function(args)
    vim.lsp.start({
      name = 'llvm_ir_lsp',
      cmd = {'llvm-ir-lsp'}, -- change this path if you have a different installation location
      root_dir = vim.fs.root(args.buf, { '.git' }) or vim.fn.getcwd(),
      filetypes = { 'llvm' },
    })
  end,
})

-- associate .ll files with llvm language
vim.filetype.add({ extension = { ll = 'llvm' } })
```

### VS Code

The recommended way is to install [Langoustine VSCode extension](https://marketplace.visualstudio.com/items?itemName=neandertech.langoustine-vscode) and then configure in your user settings:

```json
"langoustine-vscode.servers": [
    {
        "name": "LLVM IR LSP",
        "extension": "ll",
        "command": "llvm-ir-lsp",
    }
],
```
