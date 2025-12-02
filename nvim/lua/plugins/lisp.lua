vim.pack.add {
  { src = 'https://github.com/guns/vim-sexp' },
  { src = 'https://github.com/tpope/vim-sexp-mappings-for-regular-people' },
  { src = 'https://github.com/tpope/vim-repeat' },
}

-- vim-sexp configuration
-- Enable insert mode mappings
vim.g.sexp_enable_insert_mode_mappings = 1

-- Filetypes to enable vim-sexp for
vim.g.sexp_filetypes = 'clojure,scheme,lisp,timl,fennel,janet,lfe'

-- Enable Lisp indentation for auto-indent on newlines
vim.api.nvim_create_autocmd("FileType", {
  pattern = { "lisp", "scheme", "clojure", "fennel", "janet", "lfe" },
  callback = function()
    vim.bo.lisp = true
    -- Disable single quote auto-pairing in Lisp (used for quoting)
    vim.keymap.set('i', "'", "'", { buffer = true })
  end,
})
