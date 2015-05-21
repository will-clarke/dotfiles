if exists("b:did_ftplugin")
  finish
endif

setlocal shiftwidth=2

nnoremap <leader>r :RustRun<CR>
nnoremap <leader>t :RustRun! --test<CR>
