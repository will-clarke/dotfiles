" Init {{{
" initialize default settings
let s:settings = {}
let s:settings.default_indent = 2
let s:settings.max_column = 79
let s:settings.autocomplete_method = 'neocomplcache'
let s:settings.enable_cursorcolumn = 0
let s:settings.colorscheme = 'jellybeans'
if has('lua')
  let s:settings.autocomplete_method = 'neocomplete'
elseif filereadable(expand("~/.nvim/bundle/YouCompleteMe/python/ycm/youcompleteme.py"))
  let s:settings.autocomplete_method = 'ycm'
else
  NeoBundle 'Valloric/YouCompleteMe', {
        \ 'build' : {
        \     'mac' : './install.sh --clang-completer --system-libclang --omnisharp-completer',
  }
endif
" }}}

" setup & neobundle {{{
set nocompatible
set all& "reset everything to their defaults
set rtp+=~/.vim/bundle/neobundle.vim
call neobundle#begin(expand('~/.nvim/bundle/'))
NeoBundleFetch 'Shougo/neobundle.vim'
"}}}

" functions {{{
function! s:get_cache_dir(suffix) "{{{
  return resolve(expand('~/.vim/.cache' . '/' . a:suffix))
endfunction "}}}
function! Source(begin, end) "{{{
  let lines = getline(a:begin, a:end)
  for line in lines
    execute line
  endfor
endfunction "}}}
function! Preserve(command) "{{{
  " preparation: save last search, and cursor position.
  let _s=@/
  let l = line(".")
  let c = col(".")
  " do the business:
  execute a:command
  " clean up: restore previous search history, and cursor position
  let @/=_s
  call cursor(l, c)
endfunction "}}}
function! StripTrailingWhitespace() "{{{
  call Preserve("%s/\\s\\+$//e")
endfunction "}}}
function! EnsureExists(path) "{{{
  if !isdirectory(expand(a:path))
    call mkdir(expand(a:path))
  endif
endfunction "}}}
function! CloseWindowOrKillBuffer() "{{{
  let number_of_windows_to_this_buffer = len(filter(range(1, winnr('$')), "winbufnr(v:val) == bufnr('%')"))

  " never bdelete a nerd tree
  if matchstr(expand("%"), 'NERD') == 'NERD'
    wincmd c
    return
  endif

  if number_of_windows_to_this_buffer > 1
    wincmd c
  else
    bdelete
  endif
endfunction "}}}
"}}}

" base configuration {{{
let mapleader = " "
let g:mapleader = " "
set timeoutlen=300                                  "mapping timeout
set ttimeoutlen=50                                  "keycode timeout

set mouse=a                                         "enable mouse
set mousehide                                       "hide when characters are typed
set history=1000                                    "number of command lines to remember
set ttyfast                                         "assume fast terminal connection
set viewoptions=folds,options,cursor,unix,slash     "unix/windows compatibility
set encoding=utf-8                                  "set encoding for text
if exists('$TMUX')
  set clipboard=
else
  set clipboard=unnamed                             "sync with OS clipboard
endif
set hidden                                          "allow buffer switching without saving
set autoread                                        "auto reload if file saved externally
set fileformats+=mac                                "add mac to auto-detection of file format line endings
set nrformats-=octal                                "always assume decimal numbers
set showcmd
set tags=tags;/
set showfulltag
set modeline
set modelines=5

if $SHELL =~ '/fish$'
  " VIM expects to be run from a POSIX shell.
  set shell=sh
endif

set noshelltemp                                     "use pipes

" whitespace
set backspace=indent,eol,start                      "allow backspacing everything in insert mode
set autoindent                                      "automatically indent to match adjacent lines
set expandtab                                       "spaces instead of tabs
set smarttab                                        "use shiftwidth to enter tabs
let &tabstop=s:settings.default_indent              "number of spaces per tab for display
let &softtabstop=s:settings.default_indent          "number of spaces per tab in insert mode
let &shiftwidth=s:settings.default_indent           "number of spaces when indenting
set list                                            "highlight whitespace
set listchars=tab:│\ ,trail:•,extends:❯,precedes:❮
set shiftround
set linebreak
let &showbreak='↪ '

set scrolloff=5                                     "always show content after scroll
" set scrolljump=5                                    "minimum number of lines to scroll
set display+=lastline
set wildmenu                                        "show list for autocomplete
set wildmode=list:full
set wildignorecase

set splitbelow
set splitright

" disable sounds
set noerrorbells
set novisualbell
set t_vb=

" searching
set hlsearch                                        "highlight searches
set incsearch                                       "incremental searching
set ignorecase                                      "ignore case for searching
set smartcase                                       "do case-sensitive if there's a capital letter
if executable('ack')
  set grepprg=ack\ --nogroup\ --column\ --smart-case\ --nocolor\ --follow\ $*
  set grepformat=%f:%l:%c:%m
endif
if executable('ag')
  set grepprg=ag\ --nogroup\ --column\ --smart-case\ --nocolor\ --follow
  set grepformat=%f:%l:%c:%m
endif
"}}}

" vim file/folder management {{{
" persistent undo
if exists('+undofile')
  set undofile
  let &undodir = s:get_cache_dir('undo')
endif

" backups
set backup
let &backupdir = s:get_cache_dir('backup')

" swap files
let &directory = s:get_cache_dir('swap')
set noswapfile

call EnsureExists('~/.vim/.cache')
call EnsureExists(&undodir)
call EnsureExists(&backupdir)
call EnsureExists(&directory)
"}}}

" ui configuration {{{
set showmatch                                       "automatically highlight matching braces/brackets/etc.
set matchtime=2                                     "tens of a second to show matching parentheses
set number
set lazyredraw
set laststatus=2
set noshowmode
set foldenable                                      "enable folds by default
set foldmethod=syntax                               "fold via syntax of files
set foldlevelstart=99                               "open all folds by default
let g:xml_syntax_folding=1                          "enable xml folding

set cursorline
autocmd WinLeave * setlocal nocursorline
autocmd WinEnter * setlocal cursorline
let &colorcolumn=s:settings.max_column
if s:settings.enable_cursorcolumn
  set cursorcolumn
  autocmd WinLeave * setlocal nocursorcolumn
  autocmd WinEnter * setlocal cursorcolumn
endif

if has('conceal')
  set conceallevel=1
  set listchars+=conceal:Δ
endif

if $TERM_PROGRAM == 'iTerm.app'
  " different cursors for insert vs normal mode
  if exists('$TMUX')
    let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
    let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
  else
    let &t_SI = "\<Esc>]50;CursorShape=1\x7"
    let &t_EI = "\<Esc>]50;CursorShape=0\x7"
  endif
endif
"}}}

" plugin/mapping configuration {{{
NeoBundle 'matchit.zip'
NeoBundle 'bling/vim-airline' "{{{
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '¦'
let g:airline#extensions#tabline#buffer_idx_mode = 1
nmap <leader>1 <Plug>AirlineSelectTab1
nmap <leader>2 <Plug>AirlineSelectTab2
nmap <leader>3 <Plug>AirlineSelectTab3
nmap <leader>4 <Plug>AirlineSelectTab4
nmap <leader>5 <Plug>AirlineSelectTab5
nmap <leader>6 <Plug>AirlineSelectTab6
nmap <leader>7 <Plug>AirlineSelectTab7
nmap <leader>8 <Plug>AirlineSelectTab8
nmap <leader>9 <Plug>AirlineSelectTab9
"}}}
NeoBundle 'tpope/vim-surround'
NeoBundle 'tpope/vim-repeat'
NeoBundle 'tpope/vim-dispatch'
NeoBundle 'tpope/vim-eunuch'
NeoBundle 'tpope/vim-unimpaired' "{{{
nmap <c-up> [e
nmap <c-down> ]e
vmap <c-up> [egv
vmap <c-down> ]egv
"}}}
NeoBundle 'Shougo/vimproc.vim', {
      \ 'build': {
      \ 'mac': 'make -f make_mac.mak',
      \ },
      \ }
"}}}

" web - lazy {{{
NeoBundleLazy 'groenewege/vim-less', {'autoload':{'filetypes':['less']}}
NeoBundleLazy 'cakebaker/scss-syntax.vim', {'autoload':{'filetypes':['scss','sass']}}
NeoBundleLazy 'hail2u/vim-css3-syntax', {'autoload':{'filetypes':['css','scss','sass']}}
NeoBundleLazy 'ap/vim-css-color', {'autoload':{'filetypes':['css','scss','sass','less','styl']}}
NeoBundleLazy 'othree/html5.vim', {'autoload':{'filetypes':['html']}}
NeoBundleLazy 'wavded/vim-stylus', {'autoload':{'filetypes':['styl']}}
NeoBundleLazy 'digitaltoad/vim-jade', {'autoload':{'filetypes':['jade']}}
NeoBundleLazy 'juvenn/mustache.vim', {'autoload':{'filetypes':['mustache']}}
NeoBundleLazy 'gregsexton/MatchTag', {'autoload':{'filetypes':['html','xml']}}
NeoBundleLazy 'mattn/emmet-vim', {'autoload':{'filetypes':['html','xml','xsl','xslt','xsd','css','sass','scss','less','mustache']}} "{{{
function! s:zen_html_tab()
  let line = getline('.')
  if match(line, '<.*>') < 0
    return "\<c-y>,"
  endif
  return "\<c-y>n"
endfunction
autocmd FileType xml,xsl,xslt,xsd,css,sass,scss,less,mustache imap <buffer><tab> <c-y>,
autocmd FileType html imap <buffer><expr><tab> <sid>zen_html_tab()
"}}}
"}}}

" javascript - lazy {{{
NeoBundleLazy 'marijnh/tern_for_vim', {
      \ 'autoload': { 'filetypes': ['javascript'] },
      \ 'build': {
      \ 'mac': 'npm install',
      \ },
      \ }
NeoBundleLazy 'pangloss/vim-javascript', {'autoload':{'filetypes':['javascript']}}
NeoBundleLazy 'maksimr/vim-jsbeautify', {'autoload':{'filetypes':['javascript']}} "{{{
nnoremap <leader>fjs :call JsBeautify()<cr>
"}}}
NeoBundleLazy 'leafgarland/typescript-vim', {'autoload':{'filetypes':['typescript']}}
NeoBundleLazy 'kchmck/vim-coffee-script', {'autoload':{'filetypes':['coffee']}}
NeoBundleLazy 'mmalecki/vim-node.js', {'autoload':{'filetypes':['javascript']}}
NeoBundleLazy 'leshill/vim-json', {'autoload':{'filetypes':['javascript','json']}}
NeoBundleLazy 'othree/javascript-libraries-syntax.vim', {'autoload':{'filetypes':['javascript','coffee','ls','typescript']}}
"}}}

" ruby {{{
NeoBundle 'tpope/vim-rails'
NeoBundle 'tpope/vim-bundler'
"}}}

" python - lazy{{{
NeoBundleLazy 'klen/python-mode', {'autoload':{'filetypes':['python']}} "{{{
let g:pymode_rope=0
"}}}
NeoBundleLazy 'davidhalter/jedi-vim', {'autoload':{'filetypes':['python']}} "{{{
let g:jedi#popup_on_dot=0
"}}}
"}}}

" git {{{
NeoBundle 'mhinz/vim-signify' "{{{
let g:signify_update_on_bufenter=0
"}}}
if executable('hg')
  NeoBundle 'bitbucket:ludovicchabant/vim-lawrencium'
endif
NeoBundle 'tpope/vim-fugitive' "{{{
nnoremap <silent> <leader>gs :Gstatus<CR>
nnoremap <silent> <leader>gd :Gdiff<CR>
nnoremap <silent> <leader>gc :Gcommit<CR>
nnoremap <silent> <leader>gb :Gblame<CR>
nnoremap <silent> <leader>gl :Glog<CR>
nnoremap <silent> <leader>gpush :Git push<CR>
nnoremap <silent> <leader>gw :Gwrite<CR>
nnoremap <silent> <leader>gr :Gremove<CR>
autocmd BufReadPost fugitive://* set bufhidden=delete
"}}}
NeoBundleLazy 'gregsexton/gitv', {'depends':['tpope/vim-fugitive'], 'autoload':{'commands':'Gitv'}} "{{{
nnoremap <silent> <leader>gv :Gitv<CR>
nnoremap <silent> <leader>gV :Gitv!<CR>
"}}}
"}}}

" autocomplete {{{
NeoBundle 'honza/vim-snippets'
if s:settings.autocomplete_method == 'ycm' "{{{
  NeoBundle 'Valloric/YouCompleteMe' "{{{
  let g:ycm_complete_in_comments_and_strings=1
  let g:ycm_key_list_select_completion=['<C-n>', '<Down>']
  let g:ycm_key_list_previous_completion=['<C-p>', '<Up>']
  let g:ycm_filetype_blacklist={'unite': 1}
  "}}}
  NeoBundle 'SirVer/ultisnips' "{{{
  let g:UltiSnipsExpandTrigger="<tab>"
  let g:UltiSnipsJumpForwardTrigger="<tab>"
  let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
  let g:UltiSnipsSnippetsDir='~/.vim/snippets'
  "}}}
else
  NeoBundle 'Shougo/neosnippet-snippets'
  NeoBundle 'Shougo/neosnippet.vim' "{{{
  let g:neosnippet#snippets_directory='~/.vim/bundle/vim-snippets/snippets,~/.vim/snippets'
  let g:neosnippet#enable_snipmate_compatibility=1

  imap <expr><TAB> neosnippet#expandable_or_jumpable() ? "\<Plug>(neosnippet_expand_or_jump)" : (pumvisible() ? "\<C-n>" : "\<TAB>")
  smap <expr><TAB> neosnippet#expandable_or_jumpable() ? "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
  imap <expr><S-TAB> pumvisible() ? "\<C-p>" : ""
  smap <expr><S-TAB> pumvisible() ? "\<C-p>" : ""
  "}}}
endif "}}}
if s:settings.autocomplete_method == 'neocomplete' "{{{
  NeoBundleLazy 'Shougo/neocomplete.vim', {'autoload':{'insert':1}, 'vim_version':'7.3.885'} "{{{
  let g:neocomplete#enable_at_startup=1
  let g:neocomplete#data_directory=s:get_cache_dir('neocomplete')
  "}}}
endif "}}}
if s:settings.autocomplete_method == 'neocomplcache' "{{{
  NeoBundleLazy 'Shougo/neocomplcache.vim', {'autoload':{'insert':1}} "{{{
  let g:neocomplcache_enable_at_startup=1
  let g:neocomplcache_temporary_dir=s:get_cache_dir('neocomplcache')
  let g:neocomplcache_enable_fuzzy_completion=1
  "}}}
endif "}}}
"}}}

" editing {{{
NeoBundleLazy 'editorconfig/editorconfig-vim', {'autoload':{'insert':1}}
NeoBundle 'tpope/vim-endwise'
NeoBundle 'tpope/vim-speeddating'
NeoBundle 'thinca/vim-visualstar'
NeoBundle 'tomtom/tcomment_vim'
NeoBundle 'terryma/vim-expand-region' "{{{
map K <Plug>(expand_region_expand)
map J <Plug>(expand_region_shrink)
"}}}
NeoBundle 'terryma/vim-multiple-cursors'
NeoBundle 'chrisbra/NrrwRgn'
NeoBundleLazy 'godlygeek/tabular', {'autoload':{'commands':'Tabularize'}} "{{{
nmap <Leader>a& :Tabularize /&<CR>
vmap <Leader>a& :Tabularize /&<CR>
nmap <Leader>a= :Tabularize /=<CR>
vmap <Leader>a= :Tabularize /=<CR>
nmap <Leader>a: :Tabularize /:<CR>
vmap <Leader>a: :Tabularize /:<CR>
nmap <Leader>a:: :Tabularize /:\zs<CR>
vmap <Leader>a:: :Tabularize /:\zs<CR>
nmap <Leader>a, :Tabularize /,<CR>
vmap <Leader>a, :Tabularize /,<CR>
nmap <Leader>a<Bar> :Tabularize /<Bar><CR>
vmap <Leader>a<Bar> :Tabularize /<Bar><CR>
"}}}
NeoBundle 'jiangmiao/auto-pairs'
NeoBundle 'justinmk/vim-sneak' "{{{
let g:sneak#streak = 1
"}}}
"}}}

" navigation {{{
NeoBundle 'mileszs/ack.vim' "{{{
if executable('ag')
  let g:ackprg = "ag --nogroup --column --smart-case --follow"
endif
"}}}
NeoBundleLazy 'mbbill/undotree', {'autoload':{'commands':'UndotreeToggle'}} "{{{
let g:undotree_SplitLocation='botright'
let g:undotree_SetFocusWhenToggle=1
nnoremap <silent> <F5> :UndotreeToggle<CR>
"}}}
NeoBundleLazy 'EasyGrep', {'autoload':{'commands':'GrepOptions'}} "{{{
let g:EasyGrepRecursive=1
let g:EasyGrepAllOptionsInExplorer=1
let g:EasyGrepCommand=1
nnoremap <leader>vo :GrepOptions<cr>
"}}}
NeoBundle 'ctrlpvim/ctrlp.vim', { 'depends': 'tacahiroy/ctrlp-funky' } "{{{
let g:ctrlp_clear_cache_on_exit=1
let g:ctrlp_max_height=40
let g:ctrlp_show_hidden=0
let g:ctrlp_follow_symlinks=1
let g:ctrlp_max_files=20000
let g:ctrlp_cache_dir=s:get_cache_dir('ctrlp')
let g:ctrlp_reuse_window='startify'
let g:ctrlp_extensions=['funky']
let g:ctrlp_custom_ignore = {
      \ 'dir': '\v[\/]\.(git|hg|svn|idea)$',
      \ 'file': '\v\.DS_Store$'
      \ }

if executable('ag')
  let g:ctrlp_user_command='ag %s -l --nocolor -g ""'
endif

nmap \ [ctrlp]
nnoremap [ctrlp] <nop>

nnoremap [ctrlp]t :CtrlPBufTag<cr>
nnoremap [ctrlp]T :CtrlPTag<cr>
nnoremap [ctrlp]l :CtrlPLine<cr>
nnoremap [ctrlp]o :CtrlPFunky<cr>
nnoremap [ctrlp]b :CtrlPBuffer<cr>
"}}}
NeoBundleLazy 'scrooloose/nerdtree', {'autoload':{'commands':['NERDTreeToggle','NERDTreeFind']}} "{{{
let NERDTreeShowHidden=1
let NERDTreeQuitOnOpen=0
let NERDTreeShowLineNumbers=1
let NERDTreeChDirMode=0
let NERDTreeShowBookmarks=1
let NERDTreeIgnore=['\.git','\.hg']
let NERDTreeBookmarksFile=s:get_cache_dir('NERDTreeBookmarks')
nnoremap <F2> :NERDTreeToggle<CR>
nnoremap <F3> :NERDTreeFind<CR>
"}}}
NeoBundleLazy 'majutsushi/tagbar', {'autoload':{'commands':'TagbarToggle'}} "{{{
nnoremap <silent> <F9> :TagbarToggle<CR>
"}}}
"}}}

" Unite {{{
NeoBundle 'Shougo/unite.vim' "{{{
let bundle = neobundle#get('unite.vim')
function! bundle.hooks.on_source(bundle)
  call unite#filters#matcher_default#use(['matcher_fuzzy'])
  call unite#filters#sorter_default#use(['sorter_rank'])
  call unite#custom#profile('default', 'context', {
        \ 'start_insert': 1
        \ })
endfunction

let g:unite_data_directory=s:get_cache_dir('unite')
let g:unite_source_history_yank_enable=1
let g:unite_source_rec_max_cache_files=5000

if executable('ag')
  let g:unite_source_grep_command='ag'
  let g:unite_source_grep_default_opts='--nocolor --line-numbers --nogroup -S -C4'
  let g:unite_source_grep_recursive_opt=''
elseif executable('ack')
  let g:unite_source_grep_command='ack'
  let g:unite_source_grep_default_opts='--no-heading --no-color -C4'
  let g:unite_source_grep_recursive_opt=''
endif

function! s:unite_settings()
  nmap <buffer> Q <plug>(unite_exit)
  nmap <buffer> <esc> <plug>(unite_exit)
  imap <buffer> <esc> <plug>(unite_exit)
  imap <buffer> <c-j> <Plug>(unite_insert_leave)
  imap <buffer> <c-k> <Plug>(unite_insert_leave)
  nmap <buffer> <c-j> <Plug>(unite_loop_cursor_down)
  nmap <buffer> <c-k> <Plug>(unite_loop_cursor_up)
endfunction
autocmd FileType unite call s:unite_settings()

nmap <space> [unite]
nnoremap [unite] <nop>

nnoremap <silent> [unite]<space> :<C-u>Unite -toggle -auto-resize -buffer-name=mixed file_rec/async:! buffer file_mru bookmark<cr><c-u>
nnoremap <silent> [unite]f :<C-u>Unite -toggle -auto-resize -buffer-name=files file_rec/async:!<cr><c-u>
nnoremap <silent> [unite]e :<C-u>Unite -buffer-name=recent file_mru<cr>
nnoremap <silent> [unite]y :<C-u>Unite -buffer-name=yanks history/yank<cr>
nnoremap <silent> [unite]l :<C-u>Unite -auto-resize -buffer-name=line line<cr>
nnoremap <silent> [unite]b :<C-u>Unite -auto-resize -buffer-name=buffers buffer file_mru<cr>
nnoremap <silent> [unite]/ :<C-u>Unite -no-quit -buffer-name=search grep:.<cr>
nnoremap <silent> [unite]m :<C-u>Unite -auto-resize -buffer-name=mappings mapping<cr>
nnoremap <silent> [unite]s :<C-u>Unite -quick-match buffer<cr>
"}}}
NeoBundleLazy 'Shougo/neomru.vim', {'autoload':{'unite_sources':'file_mru'}}
NeoBundleLazy 'osyo-manga/unite-airline_themes', {'autoload':{'unite_sources':'airline_themes'}} "{{{
nnoremap <silent> [unite]a :<C-u>Unite -winheight=10 -auto-preview -buffer-name=airline_themes airline_themes<cr>
"}}}
NeoBundleLazy 'ujihisa/unite-colorscheme', {'autoload':{'unite_sources':'colorscheme'}} "{{{
nnoremap <silent> [unite]c :<C-u>Unite -winheight=10 -auto-preview -buffer-name=colorschemes colorscheme<cr>
"}}}
NeoBundleLazy 'tsukkee/unite-tag', {'autoload':{'unite_sources':['tag','tag/file']}} "{{{
nnoremap <silent> [unite]t :<C-u>Unite -auto-resize -buffer-name=tag tag tag/file<cr>
"}}}
NeoBundleLazy 'Shougo/unite-outline', {'autoload':{'unite_sources':'outline'}} "{{{
nnoremap <silent> [unite]o :<C-u>Unite -auto-resize -buffer-name=outline outline<cr>
"}}}
NeoBundleLazy 'Shougo/unite-help', {'autoload':{'unite_sources':'help'}} "{{{
nnoremap <silent> [unite]h :<C-u>Unite -auto-resize -buffer-name=help help<cr>
"}}}
NeoBundleLazy 'Shougo/junkfile.vim', {'autoload':{'commands':'JunkfileOpen','unite_sources':['junkfile','junkfile/new']}} "{{{
let g:junkfile#directory=s:get_cache_dir('junk')
nnoremap <silent> [unite]j :<C-u>Unite -auto-resize -buffer-name=junk junkfile junkfile/new<cr>
"}}}
"}}}

" indents {{{
NeoBundle 'nathanaelkane/vim-indent-guides' "{{{
let g:indent_guides_start_level=1
let g:indent_guides_guide_size=1
let g:indent_guides_enable_on_vim_startup=0
let g:indent_guides_color_change_percent=3
"}}}
"}}}

" text objects{{{
NeoBundle 'kana/vim-textobj-user'
NeoBundle 'kana/vim-textobj-indent'
NeoBundle 'kana/vim-textobj-entire'
NeoBundle 'lucapette/vim-textobj-underscore'
"}}}

" misc {{{
if exists('$TMUX')
  NeoBundle 'christoomey/vim-tmux-navigator'
endif
NeoBundle 'kana/vim-vspec'
NeoBundleLazy 'tpope/vim-scriptease', {'autoload':{'filetypes':['vim']}}
NeoBundleLazy 'tpope/vim-markdown', {'autoload':{'filetypes':['markdown']}}
if executable('redcarpet') && executable('instant-markdown-d')
  NeoBundleLazy 'suan/vim-instant-markdown', {'autoload':{'filetypes':['markdown']}}
endif
NeoBundleLazy 'guns/xterm-color-table.vim', {'autoload':{'commands':'XtermColorTable'}}
NeoBundle 'chrisbra/vim_faq'
NeoBundle 'vimwiki'
NeoBundle 'bufkill.vim'
NeoBundle 'mhinz/vim-startify' "{{{
let g:startify_session_dir = s:get_cache_dir('sessions')
let g:startify_change_to_vcs_root = 1
let g:startify_show_sessions = 1
nnoremap <F1> :Startify<cr>
"}}}
NeoBundle 'scrooloose/syntastic' "{{{
let g:syntastic_error_symbol = '✗'
let g:syntastic_style_error_symbol = '✠'
let g:syntastic_warning_symbol = '∆'
let g:syntastic_style_warning_symbol = '≈'
"}}}
NeoBundleLazy 'mattn/gist-vim', { 'depends': 'mattn/webapi-vim', 'autoload': { 'commands': 'Gist' } } "{{{
let g:gist_post_private=1
let g:gist_show_privates=1
"}}}
NeoBundleLazy 'Shougo/vimshell.vim', {'autoload':{'commands':[ 'VimShell', 'VimShellInteractive' ]}} "{{{
let g:vimshell_editor_command='vim'
let g:vimshell_right_prompt='getcwd()'
let g:vimshell_data_directory=s:get_cache_dir('vimshell')
let g:vimshell_vimshrc_path='~/.vim/vimshrc'

nnoremap <leader>c :VimShell -split<cr>
nnoremap <leader>cc :VimShell -split<cr>
nnoremap <leader>cn :VimShellInteractive node<cr>
nnoremap <leader>cl :VimShellInteractive lua<cr>
nnoremap <leader>cr :VimShellInteractive irb<cr>
nnoremap <leader>cp :VimShellInteractive python<cr>
"}}}
NeoBundleLazy 'zhaocai/GoldenView.Vim', {'autoload':{'mappings':['<Plug>ToggleGoldenViewAutoResize']}} "{{{
let g:goldenview__enable_default_mapping=0
nmap <F4> <Plug>ToggleGoldenViewAutoResize
"}}}
nnoremap <leader>nbu :Unite neobundle/update -vertical -no-start-insert<cr>
"}}}

" mappings {{{
" formatting shortcuts
noremap <leader>= :call Preserve("normal gg=G")<CR>
noremap <leader>+ :call StripTrailingWhitespace()<CR>j
vmap <leader>s :sort<cr>

" toggle paste
map <F6> :set invpaste<CR>:set paste?<CR>

" buffer navigation
nnoremap <Leader>h :bprev<CR>
nnoremap <Leader>l :bnext<CR>
nnoremap <Leader>d :call CloseWindowOrKillBuffer()<cr>

" smash escape
inoremap jk <esc>
inoremap kj <esc>

inoremap <C-u> <C-g>u<C-u>

if mapcheck('<space>/') == ''
  nnoremap <space>/ :vimgrep //gj **/*<left><left><left><left><left><left><left><left>
endif

" sane regex {{{
nnoremap / /\v
vnoremap / /\v
nnoremap ? ?\v
vnoremap ? ?\v
nnoremap :s/ :s/\v
" }}}

" command-line window {{{
nnoremap q: q:i
nnoremap q/ q/i
nnoremap q? q?i
" }}}

" folds {{{
nnoremap zr zr:echo &foldlevel<cr>
nnoremap zm zm:echo &foldlevel<cr>
nnoremap zR zR:echo &foldlevel<cr>
nnoremap zM zM:echo &foldlevel<cr>
" }}}

" screen line scroll
nnoremap <silent> j gj
nnoremap <silent> k gk

" auto center {{{
nnoremap <silent> n nzz
nnoremap <silent> N Nzz
nnoremap <silent> * *zz
nnoremap <silent> # #zz
nnoremap <silent> g* g*zz
nnoremap <silent> g# g#zz
nnoremap <silent> <C-o> <C-o>zz
nnoremap <silent> <C-i> <C-i>zz
"}}}

" reselect visual block after indent
vnoremap < <gv
vnoremap > >gv

" reselect last paste
nnoremap <expr> gp '`[' . strpart(getregtype(), 0, 1) . '`]'

" find current word in quickfix
nnoremap <leader>fw :execute "vimgrep ".expand("<cword>")." %"<cr>:copen<cr>
" find last search in quickfix
nnoremap <leader>ff :execute 'vimgrep /'.@/.'/g %'<cr>:copen<cr>

" shortcuts for windows {{{
nnoremap <leader>v <C-w>v<C-w>l
nnoremap <leader>s <C-w>s
" nnoremap <leader>vsa :vert sba<cr>
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
"}}}

" tab shortcuts
map <leader>tn :tabnew<CR>
map <leader>tc :tabclose<CR>

" hide annoying quit message
nnoremap <C-c> <C-c>:echo<cr>

" window killer
nnoremap <silent> Q :call CloseWindowOrKillBuffer()<cr>

" quick buffer open
nnoremap gb :ls<cr>:e #

if neobundle#is_sourced('vim-dispatch')
  nnoremap <leader>tag :Dispatch ctags -R<cr>
endif

" general
" nmap <leader>l :set list! list?<cr>

map <F10> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
      \ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
      \ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>
"}}}

" commands {{{
command! -bang Q q<bang>
command! -bang QA qa<bang>
command! -bang Qa qa<bang>
"}}}

" autocmd {{{
" go back to previous position of cursor if any
autocmd BufReadPost *
      \ if line("'\"") > 0 && line("'\"") <= line("$") |
      \  exe 'normal! g`"zvzz' |
      \ endif

autocmd FileType js,scss,css autocmd BufWritePre <buffer> call StripTrailingWhitespace()
autocmd FileType css,scss setlocal foldmethod=marker foldmarker={,}
autocmd FileType css,scss nnoremap <silent> <leader>S vi{:sort<CR>
autocmd FileType python setlocal foldmethod=indent
autocmd FileType markdown setlocal nolist
autocmd FileType vim setlocal fdm=indent keywordprg=:help
"}}}

" color schemes {{{
NeoBundle 'altercation/vim-colors-solarized' "{{{
let g:solarized_termcolors=256
let g:solarized_termtrans=1
"}}}
NeoBundle 'nanotech/jellybeans.vim'
NeoBundle 'tomasr/molokai'
NeoBundle 'chriskempson/vim-tomorrow-theme'
NeoBundle 'chriskempson/base16-vim'
NeoBundle 'w0ng/vim-hybrid'
NeoBundle 'sjl/badwolf'
NeoBundle 'zeis/vim-kolor' "{{{
let g:kolor_underlined=1
"}}}
"}}}

" my bits {{{

NeoBundle 'thoughtbot/vim-rspec' "{{{
function! RailsDetect(...) abort
  if exists('b:rails_root')
    return 1
  endif
  let fn = fnamemodify(a:0 ? a:1 : expand('%'), ':p')
  if fn =~# ':[\/]\{2\}'
    return 0
  endif
  if !isdirectory(fn)
    let fn = fnamemodify(fn, ':h')
  endif
  let file = findfile('config/environment.rb', escape(fn, ', ').';')
  if !empty(file) && isdirectory(fnamemodify(file, ':p:h:h') . '/app')
    let b:rails_root = fnamemodify(file, ':p:h:h')
    return 1
  endif
endfunction

" let g:rspec_command = "Dispatch rspec {spec}"
map <Leader>r :call RunCurrentSpecFile()<CR>
if exists("*RailsDetect") && RailsDetect()
  let g:rspec_command = 'call VtrSendCommand("zeus rspec {spec}")'
  " let g:rspec_command = "Dispatch zeus rspec {spec}"
else
  if filereadable("Gemfile")
    let g:rspec_command = 'call VtrSendCommand("bundle exec rspec {spec}")'
    " let g:rspec_command = "Dispatch bundle exec rspec {spec}"
  else
    let g:rspec_command = 'call VtrSendCommand("rspec {spec}")'
    " let g:rspec_command = "Dispatch rspec {spec}"
  endif
endif
"}}}
NeoBundle 'christoomey/vim-tmux-runner' "{{{
" noremap <leader>ka :execute "source " . "~/.nvimrc"<CR>:VtrAttachToPane<cr>
noremap <leader>ka :VtrAttachToPane<cr>
noremap <leader>kl :VtrSendLinesToRunner<cr>
vnoremap <leader>kl :VtrSendLinesToRunner<cr>
noremap <leader>kf :VtrSendFile<cr>
" noremap <leader>ks mmggVG :VtrSendLinesToRunner<cr>`m
" noremap <leader>kc :VtrSendCommandToRunner
" noremap <leader>kh :VtrFocusRunner<cr>
" noremap <leader>kk :VtrKillRunner<cr>
" noremap <leader>kr :VtrFlushCommand<cr> "reset
" noremap <leader>ko :VtrOpenRunner<cr>
" }}}
NeoBundle 'Shougo/vimfiler.vim' "{{{
let g:vimfiler_as_default_explorer = 1
" }}}

" ctrl save and quit {{{
"
inoremap <silent> <C-q> <ESC>:q<CR><ESC>
nnoremap <silent> <C-q> :q<CR>
inoremap <silent> <C-q><C-q> <ESC>:q!<CR><ESC>
nnoremap <silent> <C-q><C-q> :q!<CR>

noremap  <silent> <C-s> :update<CR><ESC>
vnoremap <silent> <C-s> <C-C>:update<CR><ESC>
inoremap <silent> <C-s> <C-O>:update<CR><ESC>
" }}}

nnoremap <Leader>vv :e ~/.nvimrc<CR>

noremap ; :
noremap : ;

noremap <leader>a mmggVGy`m
noremap <leader>ad ggVG"_d

noremap <leader>p :set paste<cr>:r !pbpaste<cr>:set nopaste<cr>

"  make <c-h> work with tmux
nnoremap <bs> :<c-u>TmuxNavigateLeft<cr>

nnoremap - :set hlsearch! hlsearch?<cr>
set nowrap

" Potentailly free leaders:
" C
" I?
" JK
" M
" N?
" O
" P?
" Q
" R
" U
" X
" Y
" Z
" }}}

" finish loading {{{

call neobundle#end()
filetype plugin indent on
syntax enable
exec 'colorscheme jellybeans'
colorscheme jellybeans

NeoBundleCheck
"}}}
