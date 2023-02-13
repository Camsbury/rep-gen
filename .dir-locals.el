((haskell-mode . ((dante-methods-alist .
                   ((new-build "cabal.project.local"
                               ("cabal" "new-repl"
                                (or dante-target (dante-package-name) nil)
                                "--builddir=dist/dante")))))))
