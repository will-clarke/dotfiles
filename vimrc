" init {{{
let mapleader = " "

set ttymouse=xterm2
set mouse=a

noremap ; :

imap jk <ESC>
noremap ; :
noremap : ;

" Use Vim settings, rather then Vi settings. This setting must be as early as
" possible, as it has side effects.

set nocompatible

" automatically rebalance windows on vim resize
autocmd VimResized * :wincmd =

set backspace=2   " Backspace deletes like most programs in insert mode
set nobackup
set nowritebackup
set noswapfile    " http://robots.thoughtbot.com/post/18739402579/global-gitignore#comment-458413287
set history=50
set ruler         " show the cursor position all the time
set showcmd       " display incomplete commands
set laststatus=2  " Always display the status line
set autowrite     " Automatically :write before running commands

set timeoutlen=500

set indentkeys-=0#            " do not break indent on #
set cinkeys-=0#

" Enable built-in matchit plugin
runtime macros/matchit.vim

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if (&t_Co > 2 || has("gui_running")) && !exists("syntaxon")
  syntax on
endif

if filereadable(expand("~/.vimrc.bundles"))
  source ~/.vimrc.bundles
endif

filetype plugin indent on

" Easier vertical movement
set so=7

" Don't redraw when executing macros
set lazyredraw

" Treat <li> and <p> tags like the block tags they are
let g:html_indent_tags = 'li\|p'

" Open new split panes to right and bottom, which feels more natural
set splitbelow
set splitright

set foldmethod=marker " foldlevel=0

" Always use vertical diffs
set diffopt+=vertical

set nowrap
" end of line
" set noeol

if !isdirectory(expand("~/.vim/bundle/Vundle\.vim"))
  !git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim
  echoe "Cloning Vundle!"
endif
" }}}
" folding {{{
nnoremap <leader>' za
" }}}
" autocmd {{{
augroup vimrcEx
  autocmd!
  " When editing a file, always jump to the last known cursor position.
  " Don't do it for commit messages, when the position is invalid, or when
  " inside an event handler (happens when dropping a file on gvim).
  autocmd BufReadPost *
        \ if &ft != 'gitcommit' && line("'\"") > 0 && line("'\"") <= line("$") |
        \   exe "normal g`\"" |
        \ endif
  " Cucumber navigation commands
  autocmd User Rails Rnavcommand step features/step_definitions -glob=**/* -suffix=_steps.rb
  autocmd User Rails Rnavcommand config config -glob=**/* -suffix=.rb -default=routes
  " Set syntax highlighting for specific file types
  autocmd BufRead,BufNewFile Appraisals set filetype=ruby
  autocmd BufRead,BufNewFile *.md set filetype=markdown
  " Enable spellchecking for Markdown
  autocmd FileType markdown setlocal spell
  " Automatically wrap at 80 characters for Markdown
  " autocmd BufRead,BufNewFile *.md setlocal textwidth=80
  " Automatically set Markdown files to wrap
  " autocmd BufRead,BufNewFile *.md setlocal wrap
  " Automatically wrap at 72 characters and spell check git commit messages
  autocmd FileType gitcommit setlocal textwidth=72
  autocmd FileType gitcommit setlocal spell
  " Allow stylesheets to autocomplete hyphenated words
  autocmd FileType css,scss,sass setlocal iskeyword+=-
augroup END
" }}}
" tabs {{{
" Softtabs, 2 spaces
set tabstop=2
set shiftwidth=2
set shiftround
set expandtab

" Useful mappings for managing tabs
nnoremap <leader>tn :tabnew<cr>
nnoremap <leader>to :tabonly<cr>
nnoremap <leader>tc :tabclose<cr>
nnoremap <leader>tm :tabmove

" Opens a new tab with the current buffer's path
" Super useful when editing files in the same directory
map <leader>te :tabedit <c-r>=expand("%:p:h")<cr>/
" }}}
" search {{{
" Visual mode pressing * or # searches for the current selection
vnoremap <silent> * :call VisualSelection('f', '')<CR>
vnoremap <silent> # :call VisualSelection('b', '')<CR>
let g:fuzzy_ignore = "*.png;*.PNG;*.JPG;*.jpg;*.GIF;*.gif;vendor/**;coverage/**;tmp/**;rdoc/**"
" Use The Silver Searcher https://github.com/ggreer/the_silver_searcher
if executable('ag')
  " Use Ag over Grep
  set grepprg=ag\ --nogroup\ --nocolor
  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
  " ag is fast enough that CtrlP doesn't need to cache
  let g:ctrlp_use_caching = 0
endif
set incsearch     " Highlight while searching
set hlsearch      " Hightlight all matches after search pattern
nnoremap <leader>h :nohlsearch<cr>
set ignorecase
set smartcase
" }}}
" pretty {{{
highlight NonText guibg=#060606
highlight Folded  guibg=#0A0A0A guifg=#9090D0

" Make it obvious where 80 characters is
set textwidth=80
set colorcolumn=+1

" Line Numbers
set number
set numberwidth=5

" Display extra whitespace
set list listchars=tab:»·,trail:·,nbsp:·

function! ToggleBackground()
  let &background = ( &background == "dark"? "light" : "dark" )
endfunction

syntax enable

let tmux_colour = readfile('/Users/wmmc/.vim/tmux_colour')
if tmux_colour == ['light']
  set background=light
else
  set background=dark
endif

if !isdirectory(expand("~/.vim/bundle/vim-colors-solarized\.vim"))
  colorscheme solarized
endif

noremap <f2> :call ToggleBackground()<CR>


function! RemoveMultipleBlankLines()
  exec '%s/^\(\s*\n\)\+/\r'
endfunction

" tmux {{{
" Change cursor depending on the Mode
function! InTmuxSession()
  return $TMUX != ''
endfunction
if InTmuxSession()
  let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
  let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
else
  let &t_SI = "\<Esc>]50;CursorShape=1\x7"
  let &t_EI = "\<Esc>]50;CursorShape=0\x7"
endif
" }}}
" }}}
" tab completion {{{
" will insert tab at beginning of line,
" will use completion if not at beginning
set wildmode=list:longest,list:full
function! InsertTabWrapper()
  let col = col('.') - 1
  if !col || getline('.')[col - 1] !~ '\k'
    return "\<tab>"
  else
    return "\<c-p>"
  endif
endfunction
inoremap <Tab> <c-r>=InsertTabWrapper()<cr>
inoremap <S-Tab> <c-n>
" }}}
" ctags {{{
" Exclude Javascript files in :Rtags via rails.vim due to warnings when parsing
let g:Tlist_Ctags_Cmd="ctags --exclude='*.js'"
" Index ctags from any project, including those outside Rails
nnoremap <Leader>ct :!ctags -R .<CR>
" }}}
" syntastic {{{
" configure syntastic syntax checking to check on open as well as save
let g:syntastic_check_on_open=1
let g:syntastic_html_tidy_ignore_errors=[" proprietary attribute \"ng-"]
" }}}
" NERD {{{
noremap <Leader>n :NERDTreeFind<CR>
noremap <Leader>m :NERDTreeToggle<CR>
let NERDTreeShowHidden = 1
let NERDTreeIgnore=['\.vim$', '\~$', '\.zeus\.sock', '.DS_Store']
" }}}
" save / quit {{{
" To remember how to force save if you have an E212 error message
ca w!!  w !sudo tee "%"

inoremap <silent> <C-q> <ESC>:q<CR><ESC>
nnoremap <silent> <C-q> :q<CR>
inoremap <silent> <C-q><C-q> <ESC>:q!<CR><ESC>
nnoremap <silent> <C-q><C-q> :q!<CR>

noremap  <silent> <C-s> :update<CR><ESC>
vnoremap <silent> <C-s> <C-C>:update<CR><ESC>
inoremap <silent> <C-s> <C-O>:update<CR><ESC>
" }}}
" inline code running {{{
" map <F4> <Plug>(xmpfilter-mark)
" map <F5> <Plug>(xmpfilter-run)
"
" imap <F4> <Plug>(xmpfilter-mark)
" imap <F5> <Plug>(xmpfilter-run)
" }}}
" clipboard {{{
" set clipboard=unnamed
noremap <leader>y :.w !pbcopy<CR><CR>
" vnoremap <leader>y :w !pbcopy<CR><CR>
vnoremap <silent><leader>y y:call system("pbcopy", getreg("\""))<CR>
noremap <leader>p :set paste<cr>:r !pbpaste<cr>:set nopaste<cr>
" vmap <leader>c :w !pbcopy<CR><CR>

let g:gist_clip_command = 'pbcopy'
" }}}
" multicursors {{{
let g:multi_cursor_next_key='<C-n>'
let g:multi_cursor_prev_key='<C-p>'
let g:multi_cursor_skip_key='<C-x>'
let g:multi_cursor_quit_key='<Esc>'
" }}}
" rails {{{
" Rails Vim Shortcuts
noremap <leader>rc :Rcontroller<CR>
noremap <leader>rg :e Gemfile<CR>
noremap <leader>rv :Rview<CR>
noremap <leader>rm :Rmigration<CR>
noremap <leader>rmd :Rmodel<CR>
noremap <leader>rmg :Rmigration<CR>
noremap <leader>rr :e config/routes.rb<CR>
noremap <leader>rsh :e spec/spec_helper.rb<CR>
noremap <leader>rs sRschema<CR>
noremap <leader>ra :A<CR>
" }}}
" projectionist {{{
if isdirectory('lib')
  let g:projectionist_heuristics = {
        \  "*": {
        \     "spec/*_spec.rb": {
        \       "type": "test",
        \       "alternate": "lib/{}.rb"
        \     },
        \     "lib/*.rb": {
        \       "type": "source",
        \       "alternate": "spec/{}_spec.rb"
        \     },
        \     "*.rb": {
        \       "type": "source",
        \       "alternate": "spec/{}_spec.rb"
        \     }
        \   }
        \ }
else
  let g:projectionist_heuristics = {
        \  "*": {
        \     "spec/*_spec.rb": {
        \       "type": "test",
        \       "alternate": "{}.rb"
        \     },
        \     "*.rb": {
        \       "type": "source",
        \       "alternate": "spec/{}_spec.rb"
        \     }
        \   }
        \ }
endif
" }}}
" buffers {{{
noremap <silent> <leader>o :bp<CR> " \p previous buffer
noremap <silent> <leader>i :bn<CR> " \n next buffer
" noremap <silent> <leader>o :BufSurfBack<CR>
" noremap <silent> <leader>i :BufSurfForward<CR>
noremap <silent> <leader>d :bwipeout<CR>
noremap <silent> <leader>b :CtrlPBuffer<cr>
" }}}
" vimrc {{{
noremap <leader>vv :execute "edit " . "~/.vimrc"<CR>
noremap <leader>vt :execute "edit " . "~/.tmux.conf"<CR>
noremap <leader>vb :execute "edit " . "~/.vimrc.bundles"<CR>
noremap <leader>vr :execute "source " . "~/.vimrc"<CR>
noremap <leader>vs :execute "source " . "~/.vimrc"<CR>
" }}}
" nifty shortcuts {{{

" Useful search for merge Conflicts:
nnoremap <leader>c /[/=/</>]\{4,}<cr>

" For wordwrap (to navigate by outputted lines rather than lines)
nmap j gj
nmap k gk

" Copy entire document (& return to same line)
nmap <leader>a mmggVG
" Prepare a :substitute command using the current word or the selected text:
nnoremap <F6> yiw:%s/\<<C-r>"\>/<C-r>"/gc<Left><Left><Left>
vnoremap <F6> y:%s/\<<C-r>"\>/<C-r>"/gc<Left><Left><Left>
" Switch between the last two files
" nnoremap <leader><leader> <c-^>
nnoremap <leader><leader> :call RunCurrentSpecFile()<CR>
" Emacs - style
inoremap <C-e> <C-o>$
inoremap <C-a> <C-o>_

" incremental up or down numbers
nnoremap <A-a> <C-a>
nnoremap <A-x> <C-x>

nnoremap <leader>w `m
nnoremap <leader>json :%!python -m json.tool<cr>

" Find file in directory
noremap <silent><leader>f :CtrlP<CR>
noremap <silent>' :CtrlPMixed<CR>

" zoom a vim pane, <C-w>= to re-balance
nnoremap <leader>- :wincmd _<cr>:wincmd \|<cr>
" nnoremap <leader>= :wincmd =<cr>

" bind K to grep word under cursor
nnoremap K :grep! "\b<C-R><C-W>\b"<CR>:cw<CR><CR>
nnoremap \ :Ag<SPACE>

noremap <leader>cd :cd %:p:h<cr>
noremap ! :!

" Close all the buffers
nnoremap <leader>bd :1,1000 bd!<cr>

noremap <leader>q :A<CR>


" move to beginning/end of line
nnoremap B ^
nnoremap E $
" }}}
" Binding.pry {{{
nmap <leader>bp orequire 'pry'; binding.pry<esc>^
nmap <leader>br orequire 'pry-remote'; binding.remote_pry<esc>^
" }}}
" potentially dodgy options {{{
" set nofoldenable " Say no to code folding...
" set relativenumber
" set hidden " Lets you 'hide' buffers
" autocmd InsertEnter,InsertLeave * set cul!
"
" bind \ (backward slash) to grep shortcut
" command -nargs=+ -complete=file -bar Ag silent! grep! <args>|cwindow|redraw!
"
" For Nerd commenter
" filetype plugin on
" }}}
" snippets {{{
let g:UltiSnipsEditSplit="vertical"
let g:UltiSnipsSnippetsDir="~/.vim/snippets"
let g:UltiSnipsSnippetDirectories=["~/.vim/snippets"]

let g:UltiSnipsExpandTrigger="<c-j>"
let g:UltiSnipsJumpForwardTrigger="<c-j>"
let g:UltiSnipsJumpBackwardTrigger="<c-k>"
" let g:UltiSnipsExpandTrigger="<tab>"
" let g:UltiSnipsListSnippets="<c-tab>"
" let g:UltiSnipsJumpForwardTrigger="<c-j>"
" let g:UltiSnipsJumpBackwardTrigger="<c-k>"

nnoremap <leader>u :UltiSnipsEdit
nnoremap <leader>us :UltiSnipsEdit<cr>
" }}}
" undo {{{
" Persistent undo
set undofile
set undodir=~/.vim/undo
nnoremap <leader>u :GundoToggle<CR>
" }}}
" goyo {{{
nnoremap <leader>go :Goyo<CR>

function! s:goyo_enter()
  if exists('$TMUX')
    silent !tmux set status off
  endif
endfunction

function! s:goyo_leave()
  if exists('$TMUX')
    silent !tmux set status on
  endif
endfunction
" }}}
"  vim tmux Runner {{{
noremap <leader>ka :execute "source " . "~/.vimrc"<CR>:VtrAttachToPane<cr>
noremap <leader>kc :VtrSendCommandToRunner
noremap <leader>kh :VtrFocusRunner<cr>
"doc
noremap <leader>kd :VtrSendFile<cr>
noremap <leader>kk :VtrKillRunner<cr>
noremap <leader>kl :VtrSendLinesToRunner<cr>
noremap <leader>kf mmggVG :VtrSendLinesToRunner<cr>`m
noremap <leader>kr :VtrFlushCommand<cr> "reset
vnoremap <leader>kl :VtrSendLinesToRunner<cr>
noremap <leader>ko :VtrOpenRunner<cr>
noremap <leader>kn :VtrOpenRunner {'orientation': 'h', 'percentage': 35}<cr>
noremap <leader>kt :VtrOpenRunner {'orientation': 'v', 'percentage': 20}<cr>
noremap <leader>kp :VtrOpenRunner {'orientation': 'h', 'percentage': 50, 'cmd': 'pry'}<cr>

let g:spec_runner_dispatcher = 'call VtrSendCommand("{command}")'

if exists("*RailsDetect") && RailsDetect()
  let g:rspec_command = 'call VtrSendCommand("zeus rspec {spec}")'
else
  if filereadable("Gemfile")
    let g:rspec_command = 'call VtrSendCommand("bundle exec rspec {spec}")'
  else
    let g:rspec_command = 'call VtrSendCommand("rspec {spec}")'
  endif
endif
" let g:rspec_command = "!rspec --drb {spec}"
inoremap <silent> <c-l> <esc>:TmuxNavigateRight<cr>
" }}}
" rspec {{{
" RSpec.vim mappings
noremap <Leader>tf :call RunCurrentSpecFile()<CR>
" noremap <Leader>tn :call RunNearestSpec()<CR>
noremap <Leader>; :call RunNearestSpec()<CR>
noremap <Leader>tl :call RunLastSpec()<CR>
noremap <Leader>ta :call RunAllSpecs()<CR>
let g:rspec_runner = "os_x_iterm"
" }}}
" funky functions {{{
" Open file in Browser
function! HandleURL()
  let s:uri = matchstr(getline("."), '[a-z]*:\/\/[^ >,;]*')
  echo s:uri
  if s:uri != ""
    exec ":!open -a 'Google Chrome' '".s:uri."'"
  else
    echo "No URI found in line."
  endif
endfunction
" }}}
" external applications {{{
" gx to open links
let g:netrw_browsex_viewer="google-chrome"
noremap <silent> <Leader>ec :call HandleURL ()<CR>
noremap <silent> <leader>em :!open -a 'Marked 2' %<cr><cr>
noremap <silent> <leader>ef :!open -a 'Google Chrome' %<cr><cr>
" }}}
" jekyll {{{
let g:jekyll_path = "~/Dropbox/dev/blog"
noremap <Leader>jb  :JekyllBuild<CR>
noremap <Leader>jn  :JekyllPost<CR>
noremap <Leader>jl  :JekyllList<CR>
let g:jekyll_post_suffix = "md"
let g:jekyll_prompt_tags = "true"
" }}}
" git {{{
noremap <leader>gc :Gcommit<CR>
noremap <leader>gs :Gstatus<CR>
noremap <leader>gw :Gwrite<CR>
noremap <leader>gl :Glog<CR>
noremap <leader>gd :Gdiff<CR>
noremap <leader>ge :Gedit
noremap <leader>gm :Gmove
noremap <leader>gb :Gbrowse
noremap <leader>gr :Gread "like gco filename
" }}}
" spelling {{{
" Pressing ,ss will toggle and untoggle spell checking
map <leader>ss :setlocal spell!<cr>
" Shortcuts using <leader>
map <leader>sn ]s
map <leader>sp [s
map <leader>sa zg
map <leader>s? z=
" Set spellfile to location that is guaranteed to exist, can be symlinked to
" Dropbox or kept in Git and managed outside of thoughtbot/dotfiles using rcm.
set spellfile=$HOME/.vim-spell-en.utf-7.add
" }}}
" relative numbers {{{
function! ToggleNuMode()
  if(&rnu =n 1)
    set nornu
  else
    set rnu
  endif
endfunction
" set nu
nnoremap <leader>rn :call ToggleNuMode()<CR>
" }}}
" Hacky way of getting <c-h> working on neovim:
if has('nvim')
  nmap <BS> <C-W>h
endif

nnoremap <leader>= mmgg=G`m
"rot13 = g?

" Ignore whitespace in DIFFs
set diffopt+=iwhite

" Escape/unescape HTML
function! HtmlEscape()
  silent s/&/\&amp;/eg
  silent s/</\&lt;/eg
  silent s/>/\&gt;/eg
endfunction

function! HtmlUnEscape()
  silent s/&lt;/</eg
  silent s/&gt;/>/eg
  silent s/&amp;/\&/eg
endfunction
" Escape/Unescape of HTML entities
noremap <leader>he :call HtmlEscape()<CR>
noremap <leader>hue :call HtmlUnEscape()<CR>

" Remove trailing whitespaces at the end of a line
function! <SID>StripTrailingWhitespaces()
  "Preparation: save last search, and cursor position.
  let _s=@/
  let l = line(".")
  let c = col(".")
  "Do the business:
  %s/\s\+$//e
  "Clean up: restore previous search history, and cursor position
  let @/=_s
  call cursor(l, c)
  :retab<CR>
endfunction

function! StripWhitespace ()
  exec ':%s/ \+$//gc'
endfunction

" Strip whitespace when saving a file
autocmd BufWritePre * :call <SID>StripTrailingWhitespaces()




"
" let g:UltiSnipsJumpForwardTrigger="<tab>"
" let g:UltiSnipsJumpBackwardTrigger="<S-tab>"
" let g:UltiSnipsExpandTrigger="<nop>"
" let g:ulti_expand_or_jump_res = 0
" function! <SID>ExpandSnippetOrReturn()
"   let snippet = UltiSnips#ExpandSnippetOrJump()
"   if g:ulti_expand_or_jump_res > 0
"     return snippet
"   else
"     return "\<C-Y>"
"   endif
" endfunction
" imap <expr> <CR> pumvisible() ? "<C-R>=<SID>ExpandSnippetOrReturn()<CR>" : "\<CR>"

nnoremap <leader>tb= Tabularize /=<cr>
"This will tabularize in the character under cursor
nnoremap <leader>tbc xP:Tabularize /<C-R>-<CR>
"This will tabularize in character set which was visually selected
vnoremap <leader>tb xP:Tabularize /<C-R>-<CR>
