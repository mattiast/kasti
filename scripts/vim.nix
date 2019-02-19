{ pkgs, python }:
let
  myVim1 = pkgs.vim_configurable.override { python = python; };
  myVim = myVim1.customize {
    name = "vim";
    vimrcConfig.packages.myVimPackage = with pkgs.vimPlugins; {
      start = [
        vim-fugitive
        vim-nix
        jedi-vim
        vim-unimpaired
        gitgutter
        colorsamplerpack
        fzf-vim
        ctrlp
        LanguageClient-neovim
        airline
      ];
    };
    vimrcConfig.customRC = ''
      set t_Co=256
      colorscheme inkpot
      let mapleader = ","
      nnoremap <silent> <leader>w :w<cr>

      set wildmenu
      set tabstop=4 shiftwidth=4 softtabstop=4 expandtab
      set wildignorecase
      set ignorecase
      set hidden
      set mouse=a
      set relativenumber
      set diffopt=filler,vertical
      set updatetime=100

      let g:LanguageClient_serverCommands = {
        \ 'python': ['pyls']
        \ }
      highlight ALEWarning ctermfg=Red ctermbg=Yellow
      highlight ALEError ctermfg=White ctermbg=Red

    '';
  };
in myVim
