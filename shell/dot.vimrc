"~/.vimrc ou /etc/vim/vimrc
"Main configuration file for vim 7
"Revision -- 4 septembre 2009
"
"Pathogen
"call pathogen#infect()


filetype indent plugin on       "Activate ViM plugins
set nocompatible                "Deactivate compatibility mode

set encoding=utf-8              "Encoding
set fileencoding=utf-8

syntax on                       "Activate syntax coloring
set title                       "Set the terminal window title
set ruler                       "Show lines and columns
set number                      "Activate line numbering
set mouse=a                     "Activate mouse
set backspace=indent,eol,start  "Makes backspace key more powerful
set wrap                        "Return line automatic, no horizontal scroll
set showcmd                     "Show commands in buffer in normal mode
set showmode                    "Show mode in buffer
set showmatch                   "Show corresponding closing delimiters
set showfulltag                 "Get function usage help automatically
set list                        "Show invisible marks
set listchars=tab:▶-            "Convert tabs in nice symbols
set cursorline                  "Activate horizontal bar to indicate cursor
"set cursorcolumn               "Activate vertical bar
"set visualbell                 "Use visual bell instead of beeping

set ttyfast                     "Improve rendering
set background=dark             "Background color
colorscheme koehler             "Color scheme

set autoindent                  "Copy indent from current line when starting a new line
set smartindent                 "Also recognizes some C syntax to manage the indent
"set cindent                    "More clever and is configurable to different indenting styles

set hlsearch                    "Highlight search results
set incsearch                   "Activate incremental searches
set ignorecase                  "Ignore case while searching
set smartcase                   "Except if search is case-sensitive
"set gdefault                   "Add /g when using %s

set shiftwidth=4                "Tabs should be converted to a group of 4 spaces.
set tabstop=4                   "Tab size
set softtabstop=4               "Causes backspace to delete 8 spaces=converted <TAB>
set expandtab                   "Replaces a <TAB> with spaces
set smarttab                    "Uses shiftwidth instead of tabstop at start of lines

set wildmenu                                    "Command-line autocompletion mode
set wildmode=list:longest,list:full             "Other good options
set wildignore=*.o,*.r,*.so,*.sl,*.tar,*.tgz    "Patterns to ignore when completing files

set t_kb=    "fix weird backspace behaviour
set nu!

"Code folding za-zR-zM
"function! MyFoldFunction()                      "Definition of custom folding function
"        let line = getline(v:foldstart)
"        let sub = substitute(line,'/\*\|\*/\|^\s+', '', 'g')
"        let lines = v:foldend - v:foldstart + 1
"        return v:folddashes.sub.'...'.lines.' lines...'.getline(v:foldend)
"endfunction
"set foldmethod=syntax                 "Folding by syntax
"set foldtext=MyFoldFunction()         "Folding with MyFoldFunction
set foldlevel=100                      "Open all the folding on opening file
                                       "Overload by plugin latex-suite for .tex
set foldcolumn=2                       "Left-size bar and manage foldings
" Make folding indent sensitive
set foldmethod=indent

" Highlight lines too long
augroup aspect
    autocmd BufRead * highlight OverLength ctermbg=darkgray guibg=darkgray
    autocmd BufRead * match OverLength /\%1200v.*/
    autocmd BufRead * highlight RedundantSpaces ctermbg=darkblue guibg=darkblue
    autocmd BufRead * 2match RedundantSpaces /\s\+$\| \+\ze\t/
augroup END

"Modification of automatic autocompletion to show sha-bangs
iab #i #include
iab #d #define
iab #b #!/bin/bash
iab #s #!/bin/sh
iab #r #!/usr/bin/ruby
iab #y #!/usr/bin/python
iab #l #!/usr/bin/perl

"Modification of ViM behavior depending on type of language
autocmd BufRead  *.html,*.htm set ft=html
autocmd BufRead  *.php,*.php3 set ft=php
autocmd BufRead  *.c,*.h set ft=c
autocmd FileType c,cpp,slang set cindent
autocmd FileType make set noexpandtab shiftwidth=8
autocmd BufRead  .followup,.article*,.letter,/tmp/mutt*,*.txt set ft=mail
autocmd BufRead  *.sh set ft=sh
autocmd BufRead  *.pl set ft=perl
"autocmd BufRead *.tex set ft=tex               "Documents latex, overloaded by hte plugin latex-suite
augroup filetypedetect
        autocmd BufNewFile,BufRead *.tex setlocal spelllang=en,fr
augroup END
au BufRead,BufNewFile *.psp set filetype=psplang
au! Syntax psplang source $HOME/.vim/indent/psp.vim


"Vim jump to the last position when reopening a file
autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

"Setting automatic orthograpic correction (z= to show propositions)
set spelllang=en,fr     "Default dictionaries
set spellsuggest=7      "Number of propositions by defaut
"set spell              "Desactivate on opening document

"Remap commonly used keys -- help/recherche/orthographe/numeros/viewports
nmap <F2> :vertical botright help 
map  <F2> :vertical botright help 
imap <F2> <esc> :vertical botright help 
nmap <F3> :set hlsearch!<cr> :set hlsearch?<cr>
map  <F3> :set hlsearch!<cr> :set hlsearch?<cr>
imap <F3> <esc> :set hlsearch!<cr> :set hlsearch?<cr>
nmap <F5> :lprevious       <cr>
map  <F5> :lprevious       <cr>
imap <F5> <esc> :lprevious <cr>
nmap <F6> :lnext       <cr>
map  <F6> :lnext       <cr>
imap <F6> <esc> :lnext <cr>
nmap <F7> :set spell!   <cr> :set spell?   <cr>
map  <F7> :set spell!   <cr> :set spell?   <cr>
imap <F7> <esc> :set spell!   <cr> :set spell?   <cr>
nmap <F8> :set nu!      <cr> :set nu?      <cr>
map  <F8> :set nu!      <cr> :set nu?      <cr>
imap <F8> <esc> :set nu!      <cr> :set nu?      <cr>
nmap <F9> <c-w>w
map  <F9> <c-w>w
imap <F9> <esc><c-w>w

"Navigation between tabs
"nmap <c-b> :tabprevious<cr>
"nmap <c-n> :tabnext    <cr>
"nmap <c-t> :tabnew     <cr>
map  <c-b> :tabprevious<cr>
map  <c-n> :tabnext    <cr>
map  <c-t> :tabnew     <cr>
"imap <c-b> <esc> :tabprevious<cr>
"imap <c-n> <esc> :tabnext    <cr>
"imap <c-t> <esc> :tabnew     <cr>

"Manipulation of buffers
noremap ,f :buffers   <cr>
noremap ,b :bprevious <cr>
noremap ,n :bnext     <cr>
"noremap ,a :badd 
"noremap ,d :bdelete<cr>

"Remap of plugins taglist and Nerdtree
noremap ,o :TagbarToggle  <cr>
noremap ,l :TlistToggle   <cr>
noremap ,t :NERDTreeToggle<cr>
noremap ,g :GundoToggle   <cr>
noremap ,s :SyntasticToggleMode<cr>

"Tagbar options
let Tlist_Ctags_Cmd = '/usr/bin/ctags'  " To use plugin taglist
"let g:tagbar_left = 1                  " To set pane at left

"Supertab options
"let g:SuperTabDefaultCompletionType = "<C-X><C-O>"
let g:SuperTabDefaultCompletionType = "context"
set completeopt=menuone,longest,preview

"Syntastic options
let g:syntastic_check_on_open=1
let g:syntastic_auto_loc_list=2 "Set to 1 to automatically open :Errors
"Checkers options
let g:syntastic_python_checker_args = '--rcfile=~/.pylintrc'
let g:syntastic_javascript_jsl_conf='-conf ~/.jsl.conf'

"Change highlight line style
highlight cursorline    term=bold cterm=bold   ctermbg=16 gui=bold   guibg=darkgrey
highlight cursorcolumn  term=bold cterm=bold   ctermbg=16 gui=bold   guibg=darkgrey

"Modification of theme colors
"Delete old configurations
highlight clear WhitespaceEOL
highlight clear Folded
highlight clear FoldColumn
highlight clear Search
highlight clear Comment
highlight clear Statusline
highlight clear StatusLineNC
highlight clear VertSplit
highlight clear wildmenu
highlight clear spellbad
highlight clear spelllocal
highlight clear spellcap
highlight clear spellrare
highlight clear TabLineSel
highlight clear TabLine
highlight clear TabLineFill
highlight clear Pmenu
highlight clear PmenuSel
highlight clear SignColumn
"highlight clear Todo
highlight clear Error

"Show in red spaces and tabs at the end of line
"highlight WhitespaceEOL ctermbg=blue
"match     WhitespaceEOL /\s\+$/

"MAnage folds -- fold recollapsed + foldcolumn on left margin
highlight Folded        term=bold cterm=bold ctermfg=brown gui=bold guifg=brown
highlight FoldColumn    term=bold cterm=bold ctermfg=white gui=bold guifg=white

"Highlight searches
highlight Search        term=bold cterm=bold ctermfg=white ctermbg=darkgreen guifg=white guibg=darkgreen

"Comments
highlight Comment       term=bold cterm=bold ctermfg=darkgrey guifg=darkgrey

"Current and non-current status line, highlight it
highlight StatusLine    term=bold cterm=bold ctermfg=white    ctermbg=darkgrey gui=bold guifg=white    guibg=darkgrey
highlight StatusLineNC  term=bold cterm=bold ctermfg=grey     ctermbg=darkgrey gui=bold guifg=grey     guibg=darkgrey
highlight Vertsplit     term=bold cterm=bold ctermfg=white                     gui=bold guifg=white
highlight wildmenu      term=bold cterm=bold ctermfg=white    ctermbg=darkblue gui=bold guifg=white    guibg=darkblue

"Orthographic correction
"spellbad word not recognized
"spelllocal wrong spelling for selected region (en_au, en_ca, ...)
"spellcap word not capitalised
"spellrare rare words
highlight spellbad      term=bold cterm=bold ctermfg=white ctermbg=darkred     gui=bold guifg=white guibg=darkred
highlight spelllocal    term=bold cterm=bold ctermfg=white ctermbg=brown       gui=bold guifg=white guibg=brown
highlight spellcap      term=bold cterm=bold ctermfg=white ctermbg=darkmagenta gui=bold guifg=white guibg=darkmagenta
highlight spellrare     term=bold cterm=bold ctermfg=white ctermbg=darkblue    gui=bold guifg=white guibg=darkblue

"Tab bar - selected/other tabs/inactive bar
highlight TabLineSel    term=bold cterm=bold ctermfg=white                  gui=bold guifg=white
highlight TabLine       term=bold cterm=bold ctermfg=darkblue ctermbg=white gui=bold guifg=darkblue guibg=white
highlight TabLineFill   term=bold cterm=bold                  ctermbg=white gui=bold                guibg=white

"Completion
highlight Pmenu        term=bold cterm=bold ctermfg=white ctermbg=darkblue   guifg=white guibg=darkblue
highlight PmenuSel     term=bold cterm=bold ctermfg=black ctermbg=darkyellow guifg=black guibg=darkyellow

"Signs column
highlight SignColumn   term=bold cterm=bold ctermbg=darkgrey                     guifg=red   guibg=darkgrey
"highlight Todo         term=bold cterm=bold ctermfg=white     ctermbg=darkyellow guifg=white guibg=darkyellow
highlight Error        term=bold cterm=bold ctermfg=white     ctermbg=red        guifg=white guibg=red

