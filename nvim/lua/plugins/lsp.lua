vim.pack.add {
  { src = 'https://github.com/neovim/nvim-lspconfig' },
  { src = 'https://github.com/mason-org/mason.nvim' },
}

require("mason").setup()

-- get rid of the vim warnings in lua files
vim.lsp.config('lua_ls', {
  settings = {
    Lua = {
      workspace = {
        library = vim.api.nvim_get_runtime_file("", true),
      }
    }
  }
})

vim.lsp.enable({ 'lua_ls', 'expert', 'ts_ls' })
vim.lsp.document_color.enable() -- hex codes are colored

-- Configure LSP floating windows with borders
-- vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
--   border = "rounded"
-- })
--
-- vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, {
--   border = "rounded"
-- })

-- Configure diagnostic floating windows
vim.diagnostic.config({
  float = {
    border = "rounded"
  }
})

-- LSP floating window colors to match background
vim.cmd([[
  highlight NormalFloat guibg=NONE ctermbg=NONE
  highlight FloatBorder guibg=NONE ctermbg=NONE
]])
