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

inoremap jk <esc>
inoremap JK <esc>
inoremap Jk <esc>
inoremap <silent><expr> <c-space> coc#refresh()

"sudo save
command! -nargs=0 Sw w !sudo tee % > /dev/null

autocmd vimenter * ++nested colorscheme gruvbox
au BufRead,BufNewFile *.sbt,*.sc set filetype=scala

"vimplug
call plug#begin()

Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'scalameta/coc-metals', {'do': 'yarn install --frozen-lockfile'}
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'preservim/nerdtree'
Plug 'morhetz/gruvbox'

call plug#end()

"syntastic
set statusline+=%#warningmsg#
set statusline+=%*
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

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

"gnvim font + disable transparency in gnvim
if exists('g:gnvim')
    set guifont=Fira\ Code\ Retina:h12
else
    autocmd vimenter * hi Normal guibg=NONE ctermbg=NONE
endif
