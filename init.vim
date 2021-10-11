set mouse=a
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
set smartindent
set autoindent
set number

noremap ; l
noremap l j
noremap k k
noremap j h
noremap <C-w>; <C-w>l
noremap <C-w>l <C-w>j
noremap <C-w>k <C-w>k
noremap <C-w>j <C-w>h
nnoremap <silent> K :call <SID>show_documentation()<CR>

imap <silent> <C-Space> <Plug>(completion_trigger)
inoremap jk <esc>
inoremap JK <esc>
inoremap Jk <esc>
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

filetype plugin indent on

autocmd vimenter * ++nested colorscheme gruvbox
au BufRead,BufNewFile *.sbt,*.sc set filetype=scala
au! BufNewFile,BufRead *.svelte set ft=html

"vimplug
call plug#begin()

"Plug 'neoclide/coc.nvim', {'branch': 'release'}
"Plug 'scalameta/coc-metals', {'do': 'yarn install --frozen-lockfile'}
Plug 'neovim/nvim-lspconfig'
Plug 'nvim-lua/completion-nvim'
Plug 'sbdchd/neoformat'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'preservim/nerdtree'
Plug 'morhetz/gruvbox'

call plug#end()

"lua bits
lua << EOF
local lsp = require('lspconfig')
local on_attach = function(client, bufnr)
    require('completion').on_attach(client, bufnr)

    local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
    local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end
    buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

    local opts = { noremap = true, silent = true }
    buf_set_keymap('n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
    buf_set_keymap('n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
    buf_set_keymap('n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
    buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
    buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
    buf_set_keymap('n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
    buf_set_keymap('n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
    buf_set_keymap('n', '<space>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
    buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
    buf_set_keymap('n', '<space>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
    buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
    buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
    buf_set_keymap('n', '<space>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
    buf_set_keymap("n", "<space>f", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)
end
local servers = { "rust_analyzer", "hls" }
for _, lsp_name in ipairs(servers) do
    lsp[lsp_name].setup {
        on_attach = on_attach,
        flags = { debounce_text_changes = 150 }
    }
end
EOF

"airline
let g:airline_symbols = {}
let g:airline_powerline_fonts = 1

let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = ''

"completion-nvim
let g:completion_enable_auto_popup = 0
let g:completion_matching_ignore_case = 1

"gnvim font + disable transparency in gnvim
if exists('g:gnvim')
    set guifont=Fira\ Code\ Retina:h12
else
    autocmd vimenter * hi Normal guibg=NONE ctermbg=NONE
endif

