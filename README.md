# Emacs config

My emacs config with some reminders

*This config use lsp mode booster, download it before running!!!*


# C/C++ LSP


To run the C/C++ lsp is needed the latest version of libstdc/libstdc++(version here) and libstdc/libstdc++(version here)-devel.

In order to achieve this, you can search in your OS. I use Ubuntu so it's something like:

```
apt-cache show libstdc++ | grep "dev"
```

And see which version is installed and try find a newer one if nothings works. I.e Mine

```
ls /usr/include/c++/  
//13 14
```

# Rust LSP

To Rust lsp works just

```
rustup component add rust-analyzer
```

# DAP

Don't forget to use these commands to dap works in emacs

```
dap-cpptols-setup
dap-codelldb-setup
```


# Latex

Install tex-live before and run
```
M-x list-packages and find auctex or M-X auctex
```


