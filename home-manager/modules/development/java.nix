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
              (let* ((buffer (efs/get-mixin-buffer))
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
                                (treesit-search-subtree node "identifier") t))
                             methods)))))
                (kill-buffer buffer)
                methods))
            
            (defun efs/insert-mixin-method ()
              "Insert a method name from the class you're mixing into."
              (interactive)
              (insert
               (concat
                "\""
                (completing-read
                 "Select a method: " (efs/get-mixin-methods))
                "\"")))
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
