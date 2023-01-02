set mouse=a
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
" set smartindent screw you smartindent!
set autoindent 
set number
set relativenumber
let mapleader = ','
let c_no_curly_error = 1 " compound literal highlighting broke

"noremap ; l
"noremap l j
"noremap k k
"noremap j h
"noremap <C-w>; <C-w>l
"noremap <C-w>l <C-w>j
"noremap <C-w>k <C-w>k
"noremap <C-w>j <C-w>h

nnoremap <leader>b :buffers<CR>:buffer<Space>
nnoremap <leader>d :buffers<CR>:bdelete<Space>
nnoremap <leader>t :bnext<CR>
nnoremap <leader>T :bprevious<CR>

inoremap jk <esc>
inoremap Jk <esc>
inoremap jK <esc>

filetype plugin indent on

autocmd vimenter * ++nested colorscheme gruvbox
autocmd InsertEnter * :set norelativenumber
autocmd InsertLeave * :set relativenumber
autocmd BufNewFile,BufRead *.sbt,*.sc set filetype=scala
autocmd! BufNewFile,BufRead *.svelte set filetype=html

" vimplug
call plug#begin()

Plug 'neovim/nvim-lspconfig'
Plug 'hrsh7th/nvim-cmp'
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-buffer'

Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'nvim-treesitter/nvim-treesitter-textobjects'

Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim', { 'tag': '0.1.0' }

Plug 'crispgm/nvim-tabline'
Plug 'nvim-lualine/lualine.nvim'

Plug 'windwp/nvim-autopairs'
Plug 'sbdchd/neoformat'

Plug 'pangloss/vim-javascript'
" Plug 'neovimhaskell/haskell-vim'

Plug 'morhetz/gruvbox'

call plug#end()

"haskell-vim
let g:haskell_indent_case = 4
let g:haskell_indent_before_where = 2
let g:haskell_indent_after_bare_where = 2
let g:haskell_indent_guard = 4

"lua bits
lua << EOF
local cmp = require'cmp'

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
        ['<CR>'] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
    }),
    sources = cmp.config.sources({{ name = 'nvim_lsp' }}, {{ name = 'buffer' }})
})

local lsp = require("lspconfig")
local opts = { noremap=true, silent=true }
vim.keymap.set('n', '<space>e', vim.diagnostic.open_float, opts)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist, opts)
local on_attach = function(client, bufnr)
    -- Enable completion triggered by <c-x><c-o>
    vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

    -- Mappings.
    -- See `:help vim.lsp.*` for documentation on any of the below functions
    local bufopts = { noremap=true, silent=true, buffer=bufnr }
    vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
    vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
    vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, bufopts)
    vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, bufopts)
    vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, bufopts)
    vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, bufopts)
    vim.keymap.set('n', '<space>ca', vim.lsp.buf.code_action, bufopts)
    vim.keymap.set('n', 'gr', vim.lsp.buf.references, bufopts)
    vim.keymap.set('n', '<space>f', function() vim.lsp.buf.format { async = true } end, bufopts)
end
local servers = { "rust_analyzer", "hls", "clangd" }
for _, lsp_name in ipairs(servers) do
    lsp[lsp_name].setup {
        on_attach = on_attach,
        flags = { debounce_text_changes = 150 }
    }
end

require'nvim-treesitter.configs'.setup {
    -- A list of parser names, or "all"
    ensure_installed = { "c", "rust", "haskell" },

    -- Install parsers synchronously (only applied to `ensure_installed`)
    sync_install = false,

    -- Automatically install missing parsers when entering buffer
    -- Recommendation: set to false if you don't have `tree-sitter` CLI installed locally
    auto_install = false,

    -- List of parsers to ignore installing (for "all")
    ignore_install = {},

    ---- If you need to change the installation directory of the parsers (see -> Advanced Setup)
    -- parser_install_dir = "/some/path/to/store/parsers", -- Remember to run vim.opt.runtimepath:append("/some/path/to/store/parsers")!

    highlight = {
        -- `false` will disable the whole extension
        enable = true,

        -- NOTE: these are the names of the parsers and not the filetype. (for example if you want to
        -- disable highlighting for the `tex` filetype, you need to include `latex` in this list as this is
        -- the name of the parser)
        -- list of language that will be disabled
        disable = {},

        -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
        -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
        -- Using this option may slow down your editor, and you may see some duplicate highlights.
        -- Instead of true it can also be a list of languages
        additional_vim_regex_highlighting = false,
    },
}

local npairs = require("nvim-autopairs")
npairs.setup { map_bs = true, map_cr = true }

require("tabline").setup {}
require("lualine").setup {
    options = {
        icons_enabled = false,
        theme = "gruvbox",
        component_separators = { left = "|", right = "|"},
        section_separators = { left = "", right = ""},
    }
}
EOF
