let g:dotvim_settings = {}
let g:dotvim_settings.version = 1

inoremap <silent> <C-q> <ESC>:q<CR><ESC>
nnoremap <silent> <C-q> :q<CR>
inoremap <silent> <C-q><C-q> <ESC>:q!<CR><ESC>
nnoremap <silent> <C-q><C-q> :q!<CR>

noremap ; :

noremap  <silent> <C-s> :update<CR><ESC>
vnoremap <silent> <C-s> <C-C>:update<CR><ESC>
inoremap <silent> <C-s> <C-O>:update<CR><ESC>

noremap <leader>p :set paste<cr>:r !pbpaste<cr>:set nopaste<cr>

set relativenumber


source ~/.nvim/vimrc
" source ~/.vimrc
