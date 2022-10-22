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

noremap ; l
noremap l j
noremap k k
noremap j h
noremap <C-w>; <C-w>l
noremap <C-w>l <C-w>j
noremap <C-w>k <C-w>k
noremap <C-w>j <C-w>h

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
Plug 'windwp/nvim-autopairs'
Plug 'pangloss/vim-javascript'
Plug 'neovimhaskell/haskell-vim'
Plug 'sbdchd/neoformat'
Plug 'crispgm/nvim-tabline'
Plug 'nvim-lualine/lualine.nvim'
Plug 'morhetz/gruvbox'

call plug#end()

"haskell-vim
let g:haskell_indent_case = 4
let g:haskell_indent_before_where = 2
let g:haskell_indent_after_bare_where = 2

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
local on_attach = function(client, bufnr)
    local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
    local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end
    buf_set_option("omnifunc", "v:lua.vim.lsp.omnifunc")

    local opts = { noremap = true, silent = true }
    buf_set_keymap("n", "K", "<Cmd>lua vim.lsp.buf.hover()<CR>", opts)
    buf_set_keymap("n", "<space>rn", "<cmd>lua vim.lsp.buf.rename()<CR>", opts)
    buf_set_keymap("n", "<space>e", "<cmd>lua vim.diagnostic.open_float()<CR>", opts)
end
local servers = { "rust_analyzer", "hls", "clangd" }
for _, lsp_name in ipairs(servers) do
    lsp[lsp_name].setup {
        on_attach = on_attach,
        flags = { debounce_text_changes = 150 }
    }
end

--[[local remap = vim.api.nvim_set_keymap

-- we have to remap insted of using keymap.recommended because npairs overrides cr and bs, hooray
local keys_for_coq = { "<esc>", "<c-c>", "<c-w>", "<c-u>"}
for _, key in ipairs(keys_for_coq) do
    remap("i", key, string.format("pumvisible() ? '<c-e>%s' : '%s'", key, key), { expr = true, noremap = true })
end
-- tab and shift tab are special
remap("i", "<tab>", "pumvisible() ? '<c-n>' : '<tab>'", { expr = true, noremap = true })
remap("i", "<s-tab>", "pumvisible() ? '<c-p>' : '<bs>'", { expr = true, noremap = true })

local npairs = require("nvim-autopairs")
npairs.setup { map_bs = true, map_cr = false }
_G.MUtils = {}
MUtils.CR = function()
    if vim.fn.pumvisible() ~= 0 then
        if vim.fn.complete_info { "selected" }.selected ~= -1 then
            return npairs.esc("<c-y>")
        else
            return npairs.esc("<c-e>") .. npairs.autopairs_cr()
        end
    else
        return npairs.autopairs_cr()
    end
end
MUtils.BS = function()
    if vim.fn.pumvisible() ~= 0 and vim.fn.complete_info { "mode" }.mode == "eval" then
        return npairs.esc("<c-e>") .. npairs.autopairs_bs()
    else
        return npairs.autopairs_bs()
    end
end
remap("i", "<cr>", "v:lua.MUtils.CR()", { expr = true, noremap = true })
remap("i", "<bs>", "v:lua.MUtils.BS()", { expr = true, noremap = true })]]
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
