(defgroup toggle-friend-file nil
  "Toggle between friend files."
  :group 'toggle-friend-file)

(defcustom toggle-friend-file-extension-mapping
  '(("cpp" "h")
    ("h" "cpp")
    ("haml" "yaml")
    ("yaml" "haml"))
  "mapping between file extensions"
  :type '(repeat
	  (list
	   (string :tag "from")
	   (string :tag "to")))
  :group 'toggle-friend-file)

(defcustom toggle-friend-file-path-mapping
  '(("src" "include") ("include" "src"))
  "replacements of file paths"
  :type '(repeat
	  (list
	   (string :tag "from")
	   (string :tag "to")))
  :group 'toggle-friend-file)

(defun toggle-friend-file-replace-with-first-pattern
  (patterns input)
  "takes a list of string used as regexp and replace and returns the result of the first replacement"
  (if (eq nil (first patterns))
      input
    (let* ((p (first patterns))
	   (replaced (replace-regexp-in-string (car p) (car (cdr p)) input))
	   (finished (not (string= replaced input))))
      (if finished replaced (toggle-friend-file-replace-with-first-pattern (rest patterns) input)))))

(defun toggle-friend-file-calc-friend-file
  (filename)
  "calcs the friend file name"
  (let*
      ((basename (file-name-sans-extension filename))
       (extension (file-name-extension filename))
       (new-extension (car (cdr (assoc extension toggle-friend-file-extension-mapping))))
       (res (toggle-friend-file-replace-with-first-pattern toggle-friend-file-path-mapping (concat basename "." new-extension))))
    res))

(defun toggle-friend-file ()
 "toggles between friend files (see toggle-friend-file customization group)"
 (interactive)
 (let*
     ((filename (buffer-file-name))
      (new-filename (toggle-friend-file-calc-friend-file filename)))
   (if (not (string= filename new-filename)) (find-file new-filename))
 ))

(cdr (assoc "cpp" toggle-friend-file-extension-mapping))
(require 'el-expectations)
(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "replace with one pattern")
      (expect "a23a" (toggle-friend-file-replace-with-first-pattern '(("1" "a")) "1231"))
      (desc "replace with second pattern")
      (expect "1b31" (toggle-friend-file-replace-with-first-pattern '(("a" "a") ("2" "b")) "1231"))
      (desc "test with customized vars")
      (expect '("cpp" "h") (first toggle-friend-file-extension-mapping))
      (desc "test with filename")
      (expect "include/Test.h" (toggle-friend-file-calc-friend-file "src/Test.cpp"))
      )))
      
(provide 'toggle-friend-file)
