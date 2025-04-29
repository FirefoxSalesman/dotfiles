{ config, pkgs, inputs, ... }:

{
  programs.emacs.init.usePackage = {
    gptel = {
      enable = true;
      defer = true;
      command = ["start-ollama"];
      custom.gptel-default-mode = "'org-mode";
      generalOne."efs/leader-keys" = {
        "g" = '''(:ignore t :which-key "gptel")'';
        "gs" = '''(start-ollama :which-key "start")'';
        "gp" = '''(gptel :which-key "prompt")'';
      };
      generalTwo."local-leader".gptel-mode-map = {
        "d" = '''(gptel-send :which-key "send")'';
        "m" = '''(gptel-menu :which-key "menu")'';
      };
      config = ''
        (gsetq gptel-backend (gptel-make-ollama "Ollama"
        		       :stream t
        		       :protocol "http"
        		       :host "localhost:11434"
        		       :models '(llama3.2:latest))
               gptel-max-tokens 10000000
               gptel-prompt-prefix-alist '((default . "You are a large language model and a helpful assistant. Respond concisely.")
        				   (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
        				   (writing . "You are a large language model and a writing assistant. Respond concisely.")
        				   (chat . "You are a large language model and a conversation partner. Respond concisely."))
               gptel-tools (list
                (gptel-make-tool
                 :name "create_file"                    ; javascript-style  snake_case name
                 :function (lambda (path filename content)   ; the function that runs
                             (let ((full-path (expand-file-name filename path)))
                               (with-temp-buffer
        			 (insert content)
        			 (write-file full-path))
                               (format "Created file %s in %s" filename path)))
                 :description "Create a new file with the specified content"
                 :args (list '(:name "path"             ; a list of argument specifications
          	                         :type string
          	                         :description "The directory where to create the file")
                             '(:name "filename"
          	                         :type string
          	                         :description "The name of the file to create")
                             '(:name "content"
          	                         :type string
          	                         :description "The content to write to the file"))
                 :category "filesystem")                ; An arbitrary label for grouping
                ))
        
        (defun start-ollama ()
          (interactive)
          (start-process-shell-command "start-ollama" nil "${(import ./scripts/start-ollama.nix { inherit pkgs config; })}/bin/start-ollama"))
      '';
    };

    gptel-quick = {
      enable = true;
      defer = true;
      package = epkgs: (pkgs.callPackage ./emacs/emacs-packages/gptel-quick.nix {
        inherit inputs;
        inherit (epkgs) trivialBuild gptel;
      });
      generalOne = {
        embark-general-map."?" = '''(gptel-quick :which-key "summarize")''; 
        "efs/leader-keys"."gq" = '''(gptel-quick :which-key "summarize")'';
      };
    };
  };
}
