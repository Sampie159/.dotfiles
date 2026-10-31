;;; lsp-target.el --- Exercise real clangd target switching -*- lexical-binding: t; -*-
;; Run from the dotfiles root in a shell with MinGW on PATH:
;; emacs --batch -Q -l emacs/config.el -l emacs/tests/lsp-target.el -f ert-run-tests-batch-and-exit

(require 'ert)
(setq lsp-keymap-prefix "C-c l")
(require 'lsp-mode)
(require 'lsp-clangd)

(defvar lsp-target-test--published (make-hash-table :test #'eq))

(defun lsp-target-test--wait (predicate)
  (let ((deadline (+ (float-time) 20)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (should (funcall predicate))))

(defun lsp-target-test--check (buffer platform &optional previous)
  (with-current-buffer buffer
    (let (workspace)
      (lsp-target-test--wait
       (lambda ()
         (setq workspace
               (seq-find (lambda (workspace)
                           (and (not (eq workspace previous))
                                (eq (lsp--workspace-status workspace) 'initialized)))
                         (lsp-workspaces)))))
      (lsp-target-test--wait
       (lambda () (member buffer-file-name (gethash workspace lsp-target-test--published))))
      (dolist (diagnostic (gethash buffer-file-name (lsp-diagnostics t)))
        (should-not (equal (lsp:diagnostic-severity? diagnostic) 1)))
      (let ((names (seq-map #'lsp:document-symbol-name
                            (lsp-request "textDocument/documentSymbol"
                                         (list :textDocument (lsp--text-document-identifier))))))
        (should (member (concat platform "_pause") names))
        (should-not (member (if (equal platform "windows") "linux_pause" "windows_pause") names)))
      (when (equal platform "windows")
        (save-excursion
          (goto-char (point-min))
          (search-forward "Sleep")
          (backward-char 3)
          (let* ((params (lsp--text-document-position-params))
                 (response (lsp-request "textDocument/completion" params))
                 (items (if (lsp-completion-list? response)
                            (lsp:completion-list-items response) response)))
            (should (seq-some (lambda (item)
                                (equal "Sleep" (lsp:completion-item-filter-text? item)))
                              items))
            (should (seq-some (lambda (location)
                                (string-match-p "mingw" (lsp:location-uri location)))
                              (lsp-request "textDocument/definition" params))))))
      workspace)))

(ert-deftest lsp-target-switches-c-and-cpp ()
  (let* ((directory (make-temp-file "emacs-lsp-target-test-" t))
         (process-environment (copy-sequence process-environment))
         (lsp-session-file (expand-file-name "session" directory))
         (lsp--session nil)
         (lsp-auto-guess-root t)
         (lsp-auto-configure nil)
         (lsp-enable-file-watchers nil)
         (lsp-enable-suggest-server-download nil)
         (lsp-enable-snippet nil)
         (lsp-diagnostics-provider :none)
         (lsp-response-timeout 15)
         (lsp-restart 'ignore)
         (lsp-clients-clangd-args '("--background-index=false"))
         (lsp-target--projects (make-hash-table :test #'equal))
         (lsp-target--directories nil)
         (lsp-diagnostic-filter
          (lambda (params workspace)
            (push (lsp--uri-to-path (lsp:publish-diagnostics-params-uri params))
                  (gethash workspace lsp-target-test--published))
            params))
         buffers commands other-buffer other-workspace)
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" directory))
          (make-directory (expand-file-name "include" directory))
          (make-directory (expand-file-name "config/clangd" directory) t)
          (setenv "XDG_CONFIG_HOME" (expand-file-name "config" directory))
          (with-temp-file (expand-file-name "config/clangd/config.yaml" directory)
            (insert "CompileFlags:\n  Add: [-DUSER_FLAG]\n"))
          (with-temp-file (expand-file-name "include/project.h" directory)
            (insert "#define PROJECT_HEADER 1\n"))
          (dolist (extension '("c" "cpp"))
            (let ((file (expand-file-name (concat "example." extension) directory)))
              (with-temp-file file
                (insert "#include \"project.h\"\n"
                        "#if !defined(PROJECT_FLAG) || !defined(USER_FLAG) || !defined(PROJECT_HEADER)\n"
                        "#error lost project or user flags\n#endif\n"
                        "#ifdef __cplusplus\n#include <vector>\nstd::vector<int> values;\n#endif\n"
                        "#ifdef _WIN32\n#include <windows.h>\n"
                        "typedef char correct_long_size[sizeof(long) == 4 ? 1 : -1];\n"
                        "void windows_pause(void) { Sleep(1000); }\n"
                        "#else\n#include <unistd.h>\n"
                        "typedef char correct_long_size[sizeof(long) == 8 ? 1 : -1];\n"
                        "void linux_pause(void) { sleep(1); }\n#endif\n"))
              (push `((directory . ,directory) (file . ,file)
                      (arguments . ,(vector (executable-find "clang") "-DPROJECT_FLAG"
                                            (concat "-I" directory "/include") "-c" file)))
                    commands)
              (push (find-file-noselect file) buffers)))
          (with-temp-file (expand-file-name "compile_commands.json" directory)
            (insert (json-serialize (vconcat commands))))
          (dolist (buffer buffers) (with-current-buffer buffer (lsp)))
          (make-directory (expand-file-name "other/.git" directory) t)
          (let ((file (expand-file-name "other/untouched.c" directory)))
            (with-temp-file file (insert "int untouched;\n"))
            (setq other-buffer (find-file-noselect file))
            (with-current-buffer other-buffer
              (lsp)
              (lsp-target-test--wait
               (lambda ()
                 (setq other-workspace (car (lsp-workspaces)))
                 (and other-workspace
                      (eq (lsp--workspace-status other-workspace) 'initialized))))))
          (let ((native (lsp-target-test--check (car buffers) "linux")))
            (lsp-target-test--check (cadr buffers) "linux")
            (with-current-buffer (car buffers)
              (goto-char (point-max))
              (insert "int unsaved_edit;\n")
              (lsp-target 'windows))
            (let ((windows (lsp-target-test--check (car buffers) "windows" native)))
              (lsp-target-test--check (cadr buffers) "windows" native)
              (let ((file (expand-file-name "new.c" directory)))
                (with-temp-file file (insert (with-current-buffer (car buffers) (buffer-string))))
                (let ((buffer (find-file-noselect file)))
                  (setq buffers (append buffers (list buffer)))
                  (with-current-buffer buffer (lsp))
                  (should (eq windows (lsp-target-test--check buffer "windows")))))
              (with-current-buffer (car buffers)
                (lsp-target 'windows)
                (should (eq windows (car (lsp-workspaces))))
                (lsp-target 'linux))
              (dolist (buffer buffers) (lsp-target-test--check buffer "linux" windows))))
          (with-current-buffer (car buffers)
            (should (buffer-modified-p))
            (should (string-match-p "unsaved_edit" (buffer-string)))
            (let ((native (car (lsp-workspaces))) (exec-path nil))
              (should-error (lsp-target 'windows) :type 'user-error)
              (should (eq native (car (lsp-workspaces)))))
            (should-error (lsp-target 'invalid) :type 'user-error)
            (should (eq (lookup-key lsp-mode-map (kbd "C-c l t")) #'lsp-target))
            (should (keymapp (lookup-key lsp-command-map (kbd "T"))))
            (let ((native (car (lsp-workspaces))))
              (lsp-target)
              (lsp-target-test--check (car buffers) "windows" native)))
          (with-current-buffer other-buffer
            (should (eq other-workspace (car (lsp-workspaces))))
            (should (process-live-p (lsp--workspace-proc other-workspace)))))
      (dolist (workspace (lsp--session-workspaces (lsp-session)))
        (lsp-workspace-shutdown workspace))
      (dolist (buffer (cons other-buffer buffers))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (lsp-target--cleanup)
      (delete-directory directory t))))

;;; lsp-target.el ends here
