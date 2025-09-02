vim.pack.add {
  { src = 'https://github.com/stevearc/conform.nvim' },
}

require("conform").setup({
  formatters_by_ft = {
    -- lua = { "stylua" },
    -- Conform will run multiple formatters sequentially
    -- python = { "isort", "black" },
    -- You can customize some of the format options for the filetype (:help conform.format)
    rust = { "rustfmt", lsp_format = "fallback" },
    -- Conform will run the first available formatter
    javascript = { "prettierd", "prettier", "eslint_d", stop_after_first = true },
    typescript = { "prettierd", "prettier", "eslint_d", stop_after_first = true },
    javascriptreact = { "prettierd", "prettier", "eslint_d", stop_after_first = true },
    typescriptreact = { "prettierd", "prettier", "eslint_d", stop_after_first = true },
  },
  format_on_save = {
    -- These options will be passed to conform.format()
    timeout_ms = 3000,
    lsp_format = "fallback",
  },
})