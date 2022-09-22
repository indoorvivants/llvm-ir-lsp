# LLMV IR LSP

<!--toc:start-->
- [LLMV IR LSP](#llmv-ir-lsp)
  - [Features](#features)
  - [Setup](#setup)
    - [Neovim](#neovim)
<!--toc:end-->

<sup>just look at this poor defenceless I surrounded by consonants</sup>

This is an experimental [Language Server](https://microsoft.github.io/language-server-protocol/) to work with [LLVM IR](https://www.llvm.org/docs/LangRef.html) - the `.ll` files produced by compiler frontends.

Currently it focuses on the metadata section, but the intent is to expand support to everything that the IR supports and makes sense in the context of IDE experience.

Here's a GIF so you know it's solid.

![2022-09-22 18 22 47](https://user-images.githubusercontent.com/1052965/191829020-d7fd39be-631a-427d-8c4d-c8cec8caeb6a.gif)

## Features

Current feature set is very limited, but stay tuned - like and subscribe, smash that ⭐ button.

**Metadata section**

- [x] document symbols
- [ ] go to definition
- [ ] hover

## Setup 

This part is very editor specific and needs your contributions!

Regardless, it all starts from downloading a server binary from the [Releases](https://github.com/indoorvivants/llvm-ir-lsp/releases) tab. 

### Neovim

In Nightly neovim, setting up a custom LSP is trivial:

```lua 
local lsp = vim.api.nvim_create_augroup("LSP", { clear = true })
-- assuming the binary is in /usr/local/bin/LLVM_LanguageServer
vim.api.nvim_create_autocmd("FileType", {
  group = lsp,
  pattern = "lifelines",
  callback = function()
    vim.lsp.start({
      name = "LLVM LSP",
      cmd = { '/usr/local/bin/LLVM_LanguageServer' },
    })
  end
})
```

The reason it's so simple is because there already exists a filetype named `lifelines` 
associated with `.ll` files!

