" This line should not be removed as it ensures that various options are
" properly set to work with the Vim-related packages available in Debian.
runtime! debian.vim

set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim/
call vundle#begin('~/.vim/plugin/')

" Start plugins

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" Markdown
Plugin 'godlygeek/tabular'
Plugin 'plasticboy/vim-markdown'

" End plugins

call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line


" Terminal with 256 colors
set t_Co=256

" Shell 
set shell=/bin/bash

" Set colorscheme
"let g:zenburn_transparent = 1
let g:zenburn_high_Contrast = 1
colors zenburn

" Vim5 and later versions support syntax highlighting. Uncommenting the next
" line enables syntax highlighting by default.
syntax on

" If using a dark background within the editing area and syntax highlighting
" turn on this option as well
set background=dark

" Uncomment the following to have Vim load indentation rules and plugins
" according to the detected filetype.
if has("autocmd")
  filetype plugin indent on
endif

" The following are commented out as they cause vim to behave a lot
" differently from regular Vi. They are highly recommended though.
set showcmd		    " Show (partial) command in status line.
set showmatch		  " Show matching brackets.
"set ignorecase		" Do case insensitive matching
"set smartcase		" Do smart case matching
"set incsearch		" Incremental search
"set autowrite		" Automatically save before commands like :next and :make
"set hidden       " Hide buffers when they are abandoned
"set mouse=a		  " Enable mouse usage (all modes)

" Tabstop
set expandtab
set shiftwidth=2
set softtabstop=2
set shiftwidth=2
set tabstop=2

" Show corrently mode
set showmode

" Visual bell
set visualbell

" Wrap line
" set nowrap
set wrap

" Spell check
autocmd BufNewFile,BufFilePre,BufRead *.{txt,md,markdown,Rmd} setlocal spell spelllang=en_gb

" No show mode
set noshowmode

let g:currentmode={
    \ 'n'  : 'Normal',
    \ 'no' : 'N·Operator Pending',
    \ 'v'  : 'Visual',
    \ 'V'  : 'V·Line',
    \ '^V' : 'V·Block', 
    \ 's'  : 'Select',
    \ 'S'  : 'S·Line', 
    \ '^S' : 'S·Block',
    \ 'i'  : 'Insert',
    \ 'R'  : 'Replace',
    \ 'Rv' : 'V·Replace',
    \ 'c'  : 'Command',
    \ 'cv' : 'Vim Ex',
    \ 'ce' : 'Ex',
    \ 'r'  : 'Prompt',
    \ 'rm' : 'More',
    \ 'r?' : 'Confirm',
    \ '!'  : 'Shell',
    \ 't'  : 'Terminal'}

" Status line
hi User1 ctermbg=242 ctermfg=15 cterm=bold
hi User2 ctermbg=240 ctermfg=15 cterm=none
hi User3 ctermbg=238 ctermfg=15 cterm=none
hi User4 ctermbg=236 ctermfg=15 cterm=none
set statusline=%1*\ %{toupper(g:currentmode[mode()])}\ 
set statusline+=%2*\ %f\%h%w%m%r\  " File name and flags
set statusline+=%3*\ spell:%{&spelllang}\  
set statusline+=%4*
set statusline+=%= " To right
set statusline+=\ %{&ff}\ \|\ %{strlen(&fenc)?&fenc:&enc}%{(&bomb?\",BOM\":\"\")}\ \|\ %y " File format and encoding
set statusline+=\ \|\ %c:%l\  " Column, line number
set statusline+=%3*\ %P\  " Porcentage

" Always display the status line, even if only one window is displayed
set laststatus=2

" Add spell check and automatic wrapping at 72 columns to commit message at git
autocmd FileType gitcommit setlocal spell spelllang=en_gb textwidth=72

" Markdown
" --------
"  Set Markdown file type to extensions
autocmd BufNewFile,BufFilePre,BufRead *.{md,markdown,Rmd} set filetype=markdown

" Disable folding
let g:vim_markdown_folding_disabled=1

" LaTeX math
let g:vim_markdown_math=1


