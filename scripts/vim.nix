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
      ];
    };
    vimrcConfig.customRC = ''
      let g:deoplete#enable_at_startup = 1
      let g:deoplete#enable_ignore_case = 1
      nnoremap <silent> <leader>w :w<cr>
    '';
  };
in myVim
