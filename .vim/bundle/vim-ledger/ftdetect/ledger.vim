augroup VimLedger
	autocmd!
	autocmd BufNewFile,BufRead *.ldg,*.ledger,*.journal,*.timeclock setlocal filetype=ledger
augroup END
