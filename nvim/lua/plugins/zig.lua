vim.pack.add {
  { src = 'https://github.com/ziglang/zig.vim' },
}

-- Disable zig.vim's built-in formatting in favor of ZLS formatting
vim.g.zig_fmt_autosave = 0

-- Configure ZLS (Zig Language Server)
vim.lsp.config('zls', {
  settings = {
    zls = {
      -- Use "partial" semantic tokens since Neovim provides basic syntax highlighting
      enable_semantic_tokens = "partial",
      -- Optional: enable build-on-save for diagnostics
      enable_build_on_save = true,
      -- Optional: auto-fix and organize imports on save
      enable_autofix = false,
    }
  }
})

-- Enable ZLS
vim.lsp.enable({ 'zls' })

-- Format-on-save for Zig files
vim.api.nvim_create_autocmd('BufWritePre', {
  pattern = { "*.zig", "*.zon" },
  callback = function()
    vim.lsp.buf.format()
  end,
  desc = "Format Zig files on save"
})
