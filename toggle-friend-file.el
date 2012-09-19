(defgroup tff nil
  "Toggle between Friend Files."
  :group 'tff)

(defcustom tff-extension-mapping
  '(("\\.cpp$" "h")
    ("\\.h$" "cpp")
    ("\\.c$" "h")
    ("\\.h$" "c")
    ("\\.haml$" "yaml")
    ("\\.yaml$" "haml")
    ("_spec\\.rb$" ".rb")
    ("\\.rb$" "_spec.rb")
    )
  "mapping between file extensions"
  :type '(repeat
	  (list
	   (regexp :tag "from")
	   (string :tag "to")))
  :group 'tff)

(defcustom tff-path-mapping
  '(("src" "include")
    ("include" "src")
    ("lib/" "spec/")
    ("spec/" "lib/")
    )
  "replacements of file paths"
  :type '(repeat
	  (list
	   (regexp :tag "from")
	   (string :tag "to")))
  :group 'tff)

(defun tff-replace-with-first-matching-regexp
  (patterns input)
  "iterates over patterns and return the regexp-replace of the first regexp-match"
  (if (first patterns)
    (let* ((pair (first patterns))
	   (pattern (car pair))
	   (repl (car (cdr pair)))
	   (replaced (replace-regexp-in-string pattern repl input))
	   (finished (not (string= replaced input))))
      (if finished replaced (tff-replace-with-first-matching-regexp (rest patterns) input)))
    input))

(defun tff-calc-file-name
  (ext-patterns regexp-patterns input)
  "replaces the file-extension and the regexp-patterns"
  (tff-replace-with-first-matching-regexp regexp-patterns (or (tff-replace-with-first-matching-regexp ext-patterns input) input)))

(defun tff
  ()
  "toggles between friend fiels (see tff customization group)"
  (interactive)
  (let* ((file-name (buffer-file-name))
	 (new-file-name (tff-calc-file-name tff-extension-mapping tff-path-mapping file-name)))
    (if (not (string= file-name new-file-name)) (find-file new-file-name))))

(progn
  (put 'tff-path-mapping 'safe-local-variable 'listp)
  (put 'tff-extension-mapping 'safe-local-variable 'listp)
)

(provide 'toggle-friend-file)

(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "test regexp")
      (expect ".rbtesttest" (replace-regexp-in-string "\\.rb$" "test" ".rbtest.rb"))

      (desc "unchanged when no matching extension")
      (expect "test.rb" (tff-replace-with-first-matching-regexp '(("cpp$" "h")) "test.rb"))
      (desc "changed when a extension matches")
      (expect "test.yaml" (tff-replace-with-first-matching-regexp '(("cpp$" "h")("\\.rb$" ".yaml")) "test.rb"))

      (desc "replace with first matching regexp")
      (expect "/some/path/src/test" (tff-replace-with-first-matching-regexp '(("include" "src")) "/some/path/include/test"))
      (desc "no replacement when no regexp matches")
      (expect "/some/path/include/test" (tff-replace-with-first-matching-regexp '(("abc" "def")) "/some/path/include/test"))

      (desc "combine extension and regex replacement")
      (expect "/some/path/include/test.h" (tff-calc-file-name '(("cpp" "h")) '(("src" "include")) "/some/path/src/test.cpp"))
      (desc "combine no extension match and regex replacement")
      (expect "/some/path/include/test.cc" (tff-calc-file-name '(("cpp" "h")) '(("src" "include")) "/some/path/src/test.cc"))
      (desc "combine extension match and no regex replacement")
      (expect "/some/path/src/test.h" (tff-calc-file-name '(("cpp$" "h")) '(("src2" "include")) "/some/path/src/test.cpp"))

      (desc "toggle between rb and spec file")
      (expect "/some/path/src/test_spec.rb" (tff-calc-file-name '(("\\.rb$" "_spec.rb")("_spec\\.rb$" ".rb")) '(("lib" "spec")) "/some/path/lib/test.rb"))

      (desc "toggle between c and h files")
      (expect "/some/path/src/test_spec.rb" (tff-calc-file-name '(("\\.c$" ".h")) '(("src" "include")) "/some/path/src/test.c"))

      )
    )
  )
