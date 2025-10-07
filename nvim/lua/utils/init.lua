local M = {}

-- File picker using ripgrep (shows all files, respects .gitignore)
function M.smart_file_picker()
  MiniPick.builtin.files({ tool = 'rg' })
end

return M