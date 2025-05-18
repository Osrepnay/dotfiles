set mouse=a
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
set autoindent 
set number
set relativenumber
let mapleader = ','
let maplocalleader = '\'
let c_no_curly_error = 1 " compound literal highlighting broke

filetype plugin indent on

inoremap jk <esc>
inoremap Jk <esc>
inoremap jK <esc>
inoremap JK <esc>

nnoremap <leader>bb :buffers<CR>:buffer<Space>
nnoremap <leader>bd :buffers<CR>:bdelete<Space>
nnoremap <leader>bt :bnext<CR>
nnoremap <leader>bT :bprevious<CR>

autocmd InsertEnter * :set norelativenumber
autocmd InsertLeave * :set relativenumber

" embedded lua highlighting
let g:vimsyn_embed = 'l'

let g:conjure#filetypes = ['clojure', 'lua', 'python'] " fennel', 'janet', 'hy', 'julia', 'racket', 'scheme', 'lua', 'lisp', 'python', 'sql']
" lsp should take care of this, keybinds overlap
let g:conjure#mapping#def_word = v:false
let g:conjure#mapping#doc_word = v:false

" vimplug
call plug#begin()

Plug 'neovim/nvim-lspconfig'
Plug 'hrsh7th/nvim-cmp'
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-buffer'

Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}

Plug 'windwp/nvim-autopairs'
Plug 'stevearc/conform.nvim'
Plug 'tpope/vim-sleuth'

Plug 'Olical/conjure'

Plug 'nvim-lualine/lualine.nvim'

Plug 'kwkarlwang/bufresize.nvim'

" Plug 'morhetz/gruvbox'
" https://github.com/morhetz/gruvbox/issues/459
Plug 'ellisonleao/gruvbox.nvim'

call plug#end()

colorscheme gruvbox

"lua bits
lua << EOF
local cmp = require('cmp')
local lsp = require('lspconfig')
local treesitter = require('nvim-treesitter.configs')
local npairs = require('nvim-autopairs')
local conform = require('conform')
local lualine = require('lualine')
local bufresize = require('bufresize')

cmp.setup({
    snippet = {
        expand = function() end
    },
    window = {},
    mapping = cmp.mapping.preset.insert({
        ['<Tab>'] = function(fallback)
            if cmp.visible() then
                cmp.select_next_item()
            else
                fallback()
            end
        end,
        ['<S-Tab>'] = function(fallback)
            if cmp.visible() then
                cmp.select_prev_item()
            else
                fallback()
            end
        end,
        ['<C-b>'] = cmp.mapping.scroll_docs(-4),
        ['<C-f>'] = cmp.mapping.scroll_docs(4),
        ['<C-Space>'] = cmp.mapping.complete(),
        ['<C-e>'] = cmp.mapping.abort(),
        ['<CR>'] = cmp.mapping.confirm({ select = false }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
    }),
    sources = cmp.config.sources({{ name = 'nvim_lsp' }}, {{ name = 'buffer' }})
})

local opts = { noremap = true, silent = true }
vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float, opts)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, opts)
local on_attach = function(client, bufnr)
    -- Enable completion triggered by <c-x><c-o>
    vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

    -- Mappings.
    -- See `:help vim.lsp.*` for documentation on any of the below functions
    local bufopts = { noremap = true, silent = true, buffer = bufnr }
    vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
    vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, bufopts)
    vim.keymap.set('n', 'gr', vim.lsp.buf.references, bufopts)
    vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
    vim.keymap.set('n', '<leader>D', vim.lsp.buf.type_definition, bufopts)
    vim.keymap.set('n', '<leader>rn', vim.lsp.buf.rename, bufopts)
    vim.keymap.set('n', '<leader>ca', vim.lsp.buf.code_action, bufopts)
    vim.keymap.set('n', '<leader>f', conform.format, bufopts)

    vim.keymap.set('i', '<C-k>', vim.lsp.buf.signature_help, bufopts)
end
local server_names = { 'rust_analyzer', 'hls', 'clangd', 'clojure_lsp' }
local server_opts = {}
for _, lsp_name in ipairs(server_names) do
    server_opts[lsp_name] = {
        on_attach = on_attach,
        flags = { debounce_text_changes = 150 }
    }
end
server_opts['hls'].settings = {
    haskell = {
        plugin = { rename = { config = { diff = true, crossModule = true } } },
        cabalFormattingProvider = 'cabalfmt',
        formattingProvider = 'ormolu'
    }
}
for lsp_name, lsp_opts in pairs(server_opts) do
    lsp[lsp_name].setup(lsp_opts)
end

treesitter.setup({
    ensure_installed = { 'c', 'rust', 'haskell', 'clojure' },
    sync_install = false,
    auto_install = false,
    ignore_install = {},
    highlight = {
        enable = true,
        disable = {},
        additional_vim_regex_highlighting = false,
    },
})

npairs.setup({ map_bs = true, map_cr = true })

lualine.setup({
    options = {
        icons_enabled = false,
        theme = 'gruvbox',
        component_separators = { left = '|', right = '|'},
        section_separators = { left = '', right = ''},
    }
})

conform.setup({
    formatters_by_ft = {
        clojure = { "zprint" },
        haskell = { "ormolu" },
    }
})

bufresize.setup()
EOF
