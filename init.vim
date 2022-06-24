set mouse=a
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
set smartindent
set number
set relativenumber

noremap ; l
noremap l j
noremap k k
noremap j h
noremap <C-w>; <C-w>l
noremap <C-w>l <C-w>j
noremap <C-w>k <C-w>k
noremap <C-w>j <C-w>h

inoremap jk <esc>
inoremap JK <esc>
inoremap Jk <esc>

filetype plugin indent on

autocmd vimenter * ++nested colorscheme gruvbox
autocmd InsertEnter * :set norelativenumber
autocmd InsertLeave * :set relativenumber
autocmd BufRead,BufNewFile *.sbt,*.sc set filetype=scala
autocmd! BufNewFile,BufRead *.svelte set filetype=html

" vimplug
call plug#begin()

Plug 'neovim/nvim-lspconfig'
Plug 'ms-jpq/coq_nvim', { 'branch': 'coq' }
Plug 'windwp/nvim-autopairs'
Plug 'pangloss/vim-javascript'
Plug 'neovimhaskell/haskell-vim'
Plug 'sbdchd/neoformat'
Plug 'crispgm/nvim-tabline'
Plug 'nvim-lualine/lualine.nvim'
Plug 'preservim/nerdtree'
Plug 'morhetz/gruvbox'

call plug#end()

"haskell-vim
let g:haskell_indent_case = 4
let g:haskell_indent_before_where = 2
let g:haskell_indent_after_bare_where = 2

"lua bits
lua << EOF
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

vim.g.coq_settings = {
    ["completion.always"] = false,
    ["auto_start"] = true
}

require("nvim-autopairs").setup {}

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
