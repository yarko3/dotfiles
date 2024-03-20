-- 1. Configure LSP
-- Set desired filetypes
-- To list all filetype names, see https://vi.stackexchange.com/a/14990
local nvim_lsp = require("lspconfig")
local configs = require("lspconfig.configs")

local has_local_lsp_config, local_lsp_config = pcall(require, "local_lsp_config")
if has_local_lsp_config then
  configs.lsp = {
      default_config = local_lsp_config.default_config,
  }
else
  configs.lsp = {
      default_config = {},
  }
end

-- 2. Configure CMP
vim.opt.completeopt = { "menu", "menuone", "noselect" }

-- Don't show matching
vim.opt.shortmess:append("c")

local lspkind = require("lspkind")
lspkind.init()

local cmp = require("cmp")

cmp.setup({
  mapping = cmp.mapping.preset.insert({
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<CR>'] = cmp.mapping.confirm({ select = false }),
    ['<Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
      else
        fallback()
      end
    end, { 'i', 's' }),
    ['<S-Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end, { 'i', 's' }),
  }),

  sources = {
    { name = "nvim_lsp" },
    { name = "path" },
    { name = "vim_vsnip" },
    { name = "buffer",   keyword_length = 5 },
  },

  sorting = {
    comparators = {}, -- We stop all sorting to let the lsp do the sorting
  },

  snippet = {
    expand = function(args)
      vim.fn["vsnip#anonymous"](args.body)
    end,
  },

  formatting = {
    format = lspkind.cmp_format({
      with_text = true,
      maxwidth = 40, -- half max width
      menu = {
        buffer = "[buffer]",
        nvim_lsp = "[LSP]",
        nvim_lua = "[API]",
        path = "[path]",
        vim_vsnip = "[snip]",
      },
    }),
  },

  experimental = {
    native_menu = false,
    ghost_text = true,
  },
})

vim.cmd([[
  augroup CmpZsh
    au!
    autocmd Filetype zsh lua require'cmp'.setup.buffer { sources = { { name = "zsh" }, } }
  augroup END
]])

-- 3. Set up LSP
local on_attach = function(client, bufnr)

  -- disable syntax highlighting
  client.server_capabilities.semanticTokensProvider = nil

  vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")
  if vim.lsp.formatexpr then -- Neovim v0.6.0+ only.
    vim.api.nvim_buf_set_option(bufnr, "formatexpr", "v:lua.vim.lsp.formatexpr")
  end
  if vim.lsp.tagfunc then
    vim.api.nvim_buf_set_option(bufnr, "tagfunc", "v:lua.vim.lsp.tagfunc")
  end

  local opts = { noremap = true, silent = true }
  vim.api.nvim_buf_set_keymap(bufnr, "n", "<leader>jf", "<cmd>lua vim.lsp.buf.code_action()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "<Leader>jk", "<cmd>lua vim.lsp.buf.references()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "<Leader>jj", "<cmd>lua vim.lsp.buf.definition()<CR>", opts)

  vim.api.nvim_command("augroup LSP")
  vim.api.nvim_command("autocmd!")
  if client.server_capabilities.documentFormattingProvider then
    vim.api.nvim_command("autocmd CursorHold  <buffer> lua vim.lsp.buf.document_highlight()")
    vim.api.nvim_command("autocmd CursorHoldI <buffer> lua vim.lsp.buf.document_highlight()")
    vim.api.nvim_command("autocmd CursorMoved <buffer> lua vim.lsp.util.buf_clear_references()")
  end
  vim.api.nvim_command("augroup END")
end

nvim_lsp.lsp.setup({
  capabilities = require("cmp_nvim_lsp").default_capabilities(vim.lsp.protocol.make_client_capabilities()),
  on_attach = on_attach,
})
