{
  flake.homeModules.java = { lib, ... }: {
    programs.emacs.init = {
      ide.languages = {
        kotlin.enable = false;
        java = {
          enable = true;
          moreEglot = true;
        };
        gradle.enable = true;
      };
      usePackage = {
        kotlin-ts-mode = {
          extraPackages = lib.mkForce [ ];
          eglot = lib.mkForce ''("intellij-server" "--stdio")'';
          config = ''
            (defun heks/eglot-uri-to-path-kotlin (orig-fn uri &rest args)
              (if (and (stringp uri) (string-prefix-p "jar:///" uri))
                  (apply orig-fn
                         (replace-regexp-in-string "^jar:///" "jar:file:///" uri)
                         args)
                (apply orig-fn uri args)))
            
            (defun heks/eglot-path-to-uri-kotlin (orig-fn path &rest args)
              (if (and (stringp path) (string-prefix-p "jar:file:///" path))
                  (replace-regexp-in-string "^jar:file:///" "jar:///" path)
                (apply orig-fn path args)))
            
            (if (fboundp 'eglot-uri-to-path)
                (progn
                  (advice-add
                   'eglot-uri-to-path
                   :around #'heks/eglot-uri-to-path-kotlin)
                  (advice-add
                   'eglot-path-to-uri
                   :around #'heks/eglot-path-to-uri-kotlin))
              (progn
                (advice-add
                 'eglot--uri-to-path
                 :around #'heks/eglot-uri-to-path-kotlin)
                (advice-add
                 'eglot--path-to-uri
                 :around #'heks/eglot-path-to-uri-kotlin)))
            
            
            (defun my-jsonrpc-request-kotlin-fix
                (orig-fn connection method params &rest args)
              "Fix kotlin-lsp empty newText bug by removing textEdit to trigger Eglot fallback."
              (let ((result (apply orig-fn connection method params args)))
                (when (and (eq method :textDocument/completion)
                           (derived-mode-p 'kotlin-mode 'kotlin-ts-mode)
                           result)
                  (let ((items
                         (if (vectorp result)
                             result
                           (plist-get result :items))))
                    (seq-do
                     (lambda (item)
                       (let ((text-edit (plist-get item :textEdit)))
                         ;; If the server sent an empty newText, strip textEdit completely
                         ;; so Eglot falls back to replacing the actual prefix.
                         (when (and text-edit
                                    (equal (plist-get text-edit :newText) ""))
                           (plist-put item :textEdit nil))))
                     items)))
                result))
            
            (defun my-jsonrpc-async-request-kotlin-fix
                (orig-fn connection method params &rest args)
              "Fix kotlin-lsp empty newText bug in asynchronous Eglot requests."
              (if (and (eq method :textDocument/completion)
                       (derived-mode-p 'kotlin-mode 'kotlin-ts-mode))
                  (let* ((orig-success (plist-get args :success-fn))
                         (new-success
                          (lambda (result)
                            (let ((items
                                   (if (vectorp result)
                                       result
                                     (plist-get result :items))))
                              (seq-do
                               (lambda (item)
                                 (let ((text-edit (plist-get item :textEdit)))
                                   (when (and text-edit
                                              (equal
                                               (plist-get text-edit :newText) ""))
                                     (plist-put item :textEdit nil))))
                               items))
                            (funcall orig-success result)))
                         (new-args
                          (plist-put
                           (copy-sequence args)
                           :success-fn new-success)))
                    (apply orig-fn connection method params new-args))
                (apply orig-fn connection method params args)))
            
            (advice-add 'jsonrpc-request :around #'my-jsonrpc-request-kotlin-fix)
            (advice-add
             'jsonrpc-async-request
             :around #'my-jsonrpc-async-request-kotlin-fix)
          '';
        };
        java-ts-mode = {
          preface = ''
            (defun tkj/java-decompile-class ()
              "Run the FernFlower decompiler on the current .class file using
             fernflower, and opens the decompiled Java file."
              (interactive)
              (let* ((current-file (buffer-file-name))
                     (output-dir
                      (concat (file-name-directory current-file) "decompiled/"))
                     (decompiled-file
                      (concat output-dir (file-name-base current-file) ".java"))
                     (command
                      (format "fernflower %s %s"
                              (shell-quote-argument current-file)
                              (shell-quote-argument output-dir))))
                (if (and current-file
                         (string-equal
                          (file-name-extension current-file) "class"))
                    (progn
                      (unless (file-directory-p output-dir)
                        (make-directory output-dir t))
                      (message "Running FernFlower decompiler...")
                      (shell-command command)
                      (if (file-exists-p decompiled-file)
                          (find-file decompiled-file)
                        (message "Error: Decompiled file not found at %s"
                                 decompiled-file)))
                  (message
                   "Error: This command can only be run on .class files"))))
            
            (defun efs/find-mixin-class ()
              "Find the the starting location of the current buffer's mixin class name."
              (if-let* ((class-declaration
                         (or (treesit-search-subtree
                              (treesit-buffer-root-node) "class_declaration")
                             (treesit-search-subtree
                              (treesit-buffer-root-node)
                              "interface_declaration")))
                        (modifiers
                         (treesit-search-subtree class-declaration "modifiers"))
                        (annotation
                         (treesit-search-subtree modifiers "annotation"))
                        (identifier
                         (treesit-search-subtree modifiers "identifier"))
                        (is-mixin
                         (equal (treesit-node-text identifier t) "Mixin"))
                        (annotation-argument-list
                         (treesit-search-subtree
                          annotation "annotation_argument_list"))
                        (class-literal
                         (treesit-search-subtree
                          annotation-argument-list "class_literal"))
                        (type-identifier
                         (treesit-search-subtree
                          class-literal "type_identifier")))
                  (treesit-node-start type-identifier)))
            
            (defun efs/get-mixin-buffer ()
              "Return a buffer for the class you're mixing into."
              (let ((class-location (efs/find-mixin-class))
                    (point (point)))
                (when class-location
                  (goto-char class-location)
                  (let ((location-marker
                         (xref-location-marker
                          (xref-item-location
                           (car
                            (eglot--lsp-xrefs-for-method
                             :textDocument/typeDefinition))))))
                    (goto-char point)
                    (marker-buffer location-marker)))))
            
            (defun efs/get-mixin-methods ()
              "Return a list of the names of the methods in the class you're mixing into."
              (when-let* ((buffer (efs/get-mixin-buffer))
                          (methods
                           (with-current-buffer buffer
                             (if-let* ((class-declaration
                                        (treesit-search-subtree
                                         (treesit-buffer-root-node)
                                         "class_declaration"))
                                       (class-body
                                        (treesit-search-subtree
                                         (treesit-buffer-root-node) "class_body"))
                                       (methods
                                        (seq-filter
                                         (lambda (node)
                                           (equal
                                            (treesit-node-type node)
                                            "method_declaration"))
                                         (treesit-node-children class-body))))
                                 (mapcar
                                  (lambda (node)
                                    (treesit-node-text
                                     (treesit-search-subtree node "identifier")
                                     t))
                                  methods)))))
                (kill-buffer buffer)
                methods))
            
            ;https://emacs.stackexchange.com/questions/15276/how-do-i-write-a-simple-completion-at-point-functions-function
            (defun efs/mixin-completion-at-point ()
              "A capf the contains the names of methods you might mix into."
              (when-let* ((bounds (bounds-of-thing-at-point 'word))
                          (node-at-point
                           (treesit-node-at
                            (marker-last-position (point-marker))))
                          (parent (treesit-node-parent node-at-point))
                          (node (treesit-node-parent parent))
                          (is-method
                           (and (equal
                                 (treesit-node-type node) "element_value_pair")
                                (or (equal
                                     "method"
                                     (treesit-node-text
                                      (treesit-search-subtree node "identifier")
                                      t))
                                    (equal
                                     "method"
                                     (treesit-node-text
                                      (treesit-search-subtree parent "identifier")
                                      t))))))
                (list
                 (car bounds)
                 (cdr bounds)
                 (efs/get-mixin-methods)
                 :exclusive 'no)))
            
            (defun efs/gen-mixin ()
              "Create a mixin for a Minecraft mod."
              (interactive)
              (let* ((java-root
                      (string-replace
                       "." "/"
                       (with-temp-buffer
                         (insert-file-contents
                          (nix-emacs-project-file "gradle.properties"))
                         (let ((group-id
                                (substring (buffer-string)
                                           (string-match
                                            "mod_group_id" (buffer-string)))))
                           (substring group-id
                                      13
                                      (string-match "\n" group-id))))))
                     (mixin-name (read-string "Mixin name: "))
                     (resource-dir
                      (nix-emacs-project-file (concat "src/main/resources/")))
                     (for-client
                      (if (equal
                           (completing-read "Client only? " '("yes" "no")) "yes")
                          "client"
                        "mixins"))
                     (mixins-file
                      (concat
                       resource-dir
                       (car
                        (seq-filter
                         (lambda (x) (string-match "mixins\.*.json" x))
                         (directory-files resource-dir)))))
                     (hash
                      (json-parse-string
                       (with-temp-buffer
                         (insert-file-contents mixins-file)
                         (buffer-string)))))
                (puthash
                 for-client
                 (append (gethash for-client hash) (list mixin-name))
                 hash)
                (with-temp-buffer (insert (json-encode hash)) (write-file mixins-file))
                (find-file (nix-emacs-project-file
            		(concat "src/main/java/" java-root "/mixin/" mixin-name ".java")))))
          '';
          generalTwoConfig.":n".java-ts-mode-map = {
            "S" = ''`,(cmd! (nix-emacs/starred-evil-open 'evil-open-below "block_comment"))'';
            "R" = ''`,(cmd! (nix-emacs/starred-evil-open 'evil-open-above "block_comment"))'';
            "o" = "'evil-previous-visual-line";
            "O" = "'evil-scroll-up";
          };
        };
        jarchive = {
          enable = true;
          after = [
            "java-ts-mode"
            "kotlin-ts-mode"
          ];
          config = "(jarchive-mode)";
        };
      };
    };
  };
}
